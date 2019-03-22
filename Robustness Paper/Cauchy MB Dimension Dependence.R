library(ggplot2)
library(latex2exp)
library(ggthemes)


makeGraph<-function(eps=.1){
  #part of projection depth MB Cauchy Mariginals
  #realted to d1 and d0 in Zuo2004
  solveABSquantile<-Vectorize(function(k,C,q){
    
    return(pcauchy(k+C)-pcauchy(C-k)-q)
  },vectorize.args = "k")
  
  
  
  #projection depth MB Cauchy Marginals
  getCauchyBiasPD<-function(sigma,eps,d=2){
    
    au<-max(sigma,sqrt(d))
    q<-1/(2*(1-eps))
    d1<-qcauchy(q)
    
    #quantile of q for |Z-d1|
    d2<-uniroot(solveABSquantile,c(-400,400),C=d1,q=q)$root
    
    d0fn<-function(C){
      return(C/uniroot(solveABSquantile,c(-400,400),C=C,q=1-q)$root)
    }
    
    d0<-optimise(d0fn,c(0,d1),maximum = T)$objective
    
    
    return(au*(d1+d0*d2))
  }
  
  #Max bias for Projection Median
  #cauchy marginals, alg general
  getCB<-function(sigma,d,eps){
    
    #unit vs
    m<-10000
    
    normalize<-function(x){
      return(x/sqrt(sum(x^2)))
    }
    x1<-replicate(d,rnorm(m))
    #d by m
    x1<-apply(x1,MARGIN=1,normalize)
    #x1 are the unit vectors
    
    #q1 quantile, 1/2(1-e) quantile of projection onto u
    getFinv<-function(u){
      scale<-sum(abs(u[1:(d-1)]))+abs(u[d])*sigma
      #print(scale)
      return(qcauchy(1/(2*(1-eps)),scale=scale))
    }
    
    int<-mean(apply(x1,2,getFinv))
    return(int*d)
  }
  
  #MB for Cauchy marginals, as per Chen 2002
  getCBHM<-Vectorize(function(sigma,eps,d=2){
    return(max(sigma,sqrt(d))*tan(pi*eps/((1-eps))))
  },vectorize.args = 'd')
  
  #dimension
  ds<-2:10
  ln<-length(ds)
  
  #epsilon
  #eps<-.3
  epsilon<-rep(eps,ln)
  
  #calculate MBS
  cb<-rep(0,ln)
  for(i in 1:ln)
    cb[i]<-getCB(sigma=1,d=ds[i],eps=eps)
  pd<-rep(0,ln)
  for(i in 1:ln)
    pd[i]<-getCauchyBiasPD(sigma=1,d=ds[i],eps=eps)
  hm<-getCBHM(1,eps,d=ds)
  
  
  mat2<-data.frame(cbind(rep(epsilon,3),c(cb,pd,hm),rep(ds,3)))
  mat2<-cbind(mat2,c(rep("PM",ln),rep("PMZ",ln),rep("HM",ln)))
  names(mat2)[4]="T"
  
  
  ggplot(mat2, aes(x=X3,y=X2,color=T))+
    ylab(TeX('$\\mathbf{B}(\\mathit{T},\\epsilon,\\mathit{F})$'))+
    # geom_smooth(se=FALSE)+
    geom_line(size=2)+
    xlab("Dimension")+
    #  coord_cartesian(ylim=c(0,50)) +
    #6by6
    scale_color_economist(name=expression(italic("T")),labels=c(expression(italic("HM")),
                                                                expression(italic("M")),
                                                                expression(italic("PMZ"))))+
    theme_economist()
  
}

makeGraph(eps = .1)
makeGraph(eps = .2)
makeGraph(eps = .3)
#save as 6x6
