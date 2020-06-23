#Cauchy differences graphs


pal=c("#3366CC","#0000FF","#0000CC","#000066","#000033")

library(ggplot2)
library(latex2exp)
library(ggthemes)
#part of projection depth MB Cauchy Mariginals
#related to d1 and d0 in Zuo2004
solveABSquantile<-Vectorize(function(k,C,q){
  
  return(pcauchy(k+C)-pcauchy(C-k)-q)
},vectorize.args = "k")

#projection depth MB Cauchy Marginals
getCauchyBiasPD<-function(sigma,eps,d=5){
  
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
  m<-500000
  
  normalize<-function(x){
    return(x/sqrt(sum(x^2)))
  }
  x1<-replicate(d,rnorm(m))
  #d by m
  x1<-apply(x1,MARGIN=1,normalize)
  #x1 are the unit vectors on upper hemisphere
  
  x1[,x1[d,]<0]=-x1[,x1[d,]<0]
  
  
  #a(u)u
  getFinv<-function(u){
    #a(u)
    scale<-sum(abs(u[1:(d-1)]))+abs(u[d])*sigma
    
    #print(scale)
    return(scale*u)
  }
  
  #integral
  int_vals=apply(x1,2,getFinv); dim(int_vals)
  int_vals=rowMeans(int_vals)
  int_vals=sqrt(sum(int_vals^2))
  
  return(int_vals*d*qcauchy(1/(2*(1-eps))))
}

#MB for Cauchy marginals, as per Chen 2002
getCBHM<-Vectorize(function(sigma,eps,d=5){
  return(max(sigma,sqrt(d))*tan(pi*eps/(1-eps)))
},vectorize.args = 'd')


#epsilons
e_length=100
epss<-seq(.01,.45,length.out = e_length)

sigmas<-c(1,2,5,8,10)

#projection depth mb
biasesPD<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPD[i,j]<-getCauchyBiasPD(sigmas[i],epss[j],d=2)

biasesPD

#halfspace median
biasesHM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesHM[i,j]<-getCBHM(sigmas[i],epss[j],d=2)
# biasesHM

#projection median
biasesPM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPM[i,j]<-getCB(sigmas[i],d=2,epss[j])
biasesPM




#make data frame for ggplot and plot

Epsilon<-sort(rep(epss,5))
Scale=rep(sigmas,length(epss))
Bias<-c(biasesPM-biasesPD)

dat<-data.frame(cbind(Epsilon,Scale,Bias))
dat$Scale=as.factor(dat$Scale)
ggplot(dat, aes(x=Epsilon,y=Bias,
                color=Scale,group=Scale))+
    #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  ylab(TeX('$\\mathbf{B}(PMD,\\epsilon,\\mathit{F})-\\mathbf{B}(PM_{zuo},\\epsilon,\\mathit{F})$'))+
  xlab(TeX('$\\epsilon$'))+
# coord_cartesian(ylim=c(-160,-1)) +
  scale_color_manual(values=pal)+
  theme_bw()+
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))#+
#  ggtitle("Bias Difference, PMZ")


#epsilon only goes up to .33 for HM
epss<-seq(.05,.3,length.out = e_length)

biasesHM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesHM[i,j]<-getCBHM(sigmas[i],epss[j],d=2)
biasesHM


biasesPM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPM[i,j]<-getCB(sigmas[i],d=2,epss[j])
biasesPM





Epsilon<-sort(rep(epss,5))
Scale=rep(sigmas,length(epss))
Bias<-c(biasesPM-biasesHM)
#8x9
dat<-data.frame(cbind(Epsilon,Scale,Bias))
dat$Scale=as.factor(dat$Scale)
ggplot(dat, aes(x=Epsilon,y=Bias,
                color=Scale,group=Scale))+
  ylab(TeX('$\\mathbf{B}(PMD,\\epsilon,\\mathit{F})-\\mathbf{B}(HM,\\epsilon,\\mathit{F})$'))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  xlab(TeX('$\\epsilon$'))+
  #  coord_cartesian(ylim=c(-1,4)) +
  scale_color_manual(values=pal)+
  theme_bw()+
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))#+
 # ggtitle("Bias Difference, HM")


#variation over u

a<-sigmas
grid<-seq(0,pi,length.out = 200)
f2<-Vectorize(function(val,x=grid){sum(abs(matrix(c(cos(x),sin(x)),nrow=1))*c(1,val))},vectorize.args = 'x')


tmp<-f2(1,grid)
mat<-cbind(grid,tmp,rep(1,length(grid)))
colnames(mat)<-c("Epsilon","Bias","Scale")
row.names(mat)<-NULL
dat2<-data.frame(mat)


names(dat2)<-c("Epsilon","Bias","Scale")
for(i in 1:length(a)){
  tmp<-f2(a[i],grid)
  mat<-cbind(grid,tmp,rep(a[i],length(grid)))
  colnames(mat)<-c("Epsilon","Bias","Scale")
  row.names(mat)<-NULL
  #  inff<-c(0.5,1000,a[i])
  dat2<-rbind(dat2,mat)
}




dat2$Scale=as.factor(dat2$Scale)
ggplot(dat2, aes(x=Epsilon,y=Bias,
                 color=Scale,group=Scale))+
  ylab(TeX('$a(u)$'))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  #coord_cartesian(ylim=c(-20,0.15)) +
  #  theme(legend.position = c(2.5, 20))+
  xlab("Spherical Angle")+
   theme_bw()+
  scale_color_manual(values=pal)+
  
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))
#+
  #ggtitle("Variation over u")




################d=5

epss<-seq(.01,.45,length.out = e_length)
#projection depth mb
biasesPD<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPD[i,j]<-getCauchyBiasPD(sigmas[i],epss[j],d=5)

biasesPD

#halfspace median
biasesHM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesHM[i,j]<-getCBHM(sigmas[i],epss[j],d=5)
biasesHM

#projection median
biasesPM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPM[i,j]<-getCB(sigmas[i],d=5,epss[j])
biasesPM




#make data frame for ggplot and plot

Epsilon<-sort(rep(epss,5))
Scale=rep(sigmas,length(epss))
Bias<-c(biasesPM-biasesPD)

dat<-data.frame(cbind(Epsilon,Scale,Bias))
dat$Scale=as.factor(dat$Scale)
ggplot(dat, aes(x=Epsilon,y=Bias,
                color=Scale,group=Scale))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  ylab(TeX('$\\mathbf{B}(PMD,\\epsilon,\\mathit{F})-\\mathbf{B}(PM_{zuo},\\epsilon,\\mathit{F})$'))+
  xlab(TeX('$\\epsilon$'))+
  # coord_cartesian(ylim=c(-160,-1)) +
  scale_color_manual(values=pal)+
  theme_bw()+
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))#+
#  ggtitle("Bias Difference, PMZ")


#epsilon only goes up to .33 for HM
epss<-seq(.05,.3,length.out = e_length)

biasesHM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesHM[i,j]<-getCBHM(sigmas[i],epss[j],d=5)
biasesHM


biasesPM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPM[i,j]<-getCB(sigmas[i],d=5,epss[j])
biasesPM





Epsilon<-sort(rep(epss,5))
Scale=rep(sigmas,length(epss))
Bias<-c(biasesPM-biasesHM)
#8x9
dat<-data.frame(cbind(Epsilon,Scale,Bias))
dat$Scale=as.factor(dat$Scale)
ggplot(dat, aes(x=Epsilon,y=Bias,
                color=Scale,group=Scale))+
  ylab(TeX('$\\mathbf{B}(PMD,\\epsilon,\\mathit{F})-\\mathbf{B}(HM,\\epsilon,\\mathit{F})$'))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  xlab(TeX('$\\epsilon$'))+
  #  coord_cartesian(ylim=c(-1,4)) +
  scale_color_manual(values=pal)+
  theme_bw()+
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))#+
# ggtitle("Bias Difference, HM")


#variation over u

a<-sigmas
grid<-seq(0,pi,length.out = 200)
f2<-Vectorize(function(val,x=grid){sum(abs(matrix(c(cos(x),sin(x)),nrow=1))*c(1,val))},vectorize.args = 'x')


tmp<-f2(1,grid)
mat<-cbind(grid,tmp,rep(1,length(grid)))
colnames(mat)<-c("Epsilon","Bias","Scale")
row.names(mat)<-NULL
dat2<-data.frame(mat)


names(dat2)<-c("Epsilon","Bias","Scale")
for(i in 1:length(a)){
  tmp<-f2(a[i],grid)
  mat<-cbind(grid,tmp,rep(a[i],length(grid)))
  colnames(mat)<-c("Epsilon","Bias","Scale")
  row.names(mat)<-NULL
  #  inff<-c(0.5,1000,a[i])
  dat2<-rbind(dat2,mat)
}




dat2$Scale=as.factor(dat2$Scale)
ggplot(dat2, aes(x=Epsilon,y=Bias,
                 color=Scale,group=Scale))+
  ylab(TeX('$a(u)$'))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  #coord_cartesian(ylim=c(-20,0.15)) +
  theme_economist()+ 
  #  theme(legend.position = c(2.5, 20))+
  xlab("Spherical Angle")+
  scale_color_manual(values=pal)+
  
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))
#+
#ggtitle("Variation over u")


################d=10
epss<-seq(.01,.45,length.out = e_length)

#projection depth mb
biasesPD<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPD[i,j]<-getCauchyBiasPD(sigmas[i],epss[j],d=10)

biasesPD

#halfspace median
biasesHM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesHM[i,j]<-getCBHM(sigmas[i],epss[j],d=10)
biasesHM

#projection median
biasesPM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPM[i,j]<-getCB(sigmas[i],d=10,epss[j])
biasesPM




#make data frame for ggplot and plot

Epsilon<-sort(rep(epss,5))
Scale=rep(sigmas,length(epss))
Bias<-c(biasesPM-biasesPD)

dat<-data.frame(cbind(Epsilon,Scale,Bias))
dat$Scale=as.factor(dat$Scale)
ggplot(dat, aes(x=Epsilon,y=Bias,
                color=Scale,group=Scale))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  ylab(TeX('$\\mathbf{B}(PMD,\\epsilon,\\mathit{F})-\\mathbf{B}(PM_{zuo},\\epsilon,\\mathit{F})$'))+
  xlab(TeX('$\\epsilon$'))+
  # coord_cartesian(ylim=c(-160,-1)) +
  scale_color_manual(values=pal)+
  theme_bw()+
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))#+
#  ggtitle("Bias Difference, PMZ")


#epsilon only goes up to .33 for HM
epss<-seq(.05,.3,length.out = e_length)

biasesHM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesHM[i,j]<-getCBHM(sigmas[i],epss[j],d=10)
biasesHM


biasesPM<-matrix(0,nrow=length(sigmas),ncol=e_length)
for(i in 1:length(sigmas))
  for(j in 1:e_length)
    biasesPM[i,j]<-getCB(sigmas[i],d=10,epss[j])
biasesPM





Epsilon<-sort(rep(epss,5))
Scale=rep(sigmas,length(epss))
Bias<-c(biasesPM-biasesHM)
#8x9
dat<-data.frame(cbind(Epsilon,Scale,Bias))
dat$Scale=as.factor(dat$Scale)
ggplot(dat, aes(x=Epsilon,y=Bias,
                color=Scale,group=Scale))+
  ylab(TeX('$\\mathbf{B}(PMD,\\epsilon,\\mathit{F})-\\mathbf{B}(HM,\\epsilon,\\mathit{F})$'))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  xlab(TeX('$\\epsilon$'))+
  #  coord_cartesian(ylim=c(-1,4)) +
  scale_color_manual(values=pal)+
  theme_bw()+
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))#+
# ggtitle("Bias Difference, HM")


#variation over u

a<-sigmas
grid<-seq(0,pi,length.out = 200)
f2<-Vectorize(function(val,x=grid){sum(abs(matrix(c(cos(x),sin(x)),nrow=1))*c(1,val))},vectorize.args = 'x')


tmp<-f2(1,grid)
mat<-cbind(grid,tmp,rep(1,length(grid)))
colnames(mat)<-c("Epsilon","Bias","Scale")
row.names(mat)<-NULL
dat2<-data.frame(mat)


names(dat2)<-c("Epsilon","Bias","Scale")
for(i in 1:length(a)){
  tmp<-f2(a[i],grid)
  mat<-cbind(grid,tmp,rep(a[i],length(grid)))
  colnames(mat)<-c("Epsilon","Bias","Scale")
  row.names(mat)<-NULL
  #  inff<-c(0.5,1000,a[i])
  dat2<-rbind(dat2,mat)
}




dat2$Scale=as.factor(dat2$Scale)
ggplot(dat2, aes(x=Epsilon,y=Bias,
                 color=Scale,group=Scale))+
  ylab(TeX('$a(u)$'))+
  #stat_smooth(lwd=1.5,se=FALSE)+
  geom_line(lwd=1.5)+
  #coord_cartesian(ylim=c(-20,0.15)) +
  theme_bw()+
  #  theme(legend.position = c(2.5, 20))+
  xlab("Spherical Angle")+
  
  scale_color_manual(values=pal)+
  
  #scale_color_brewer(type = 'seq', palette ='PuRd', direction = 1)+
  theme(plot.title = element_text(size = 40, face = "bold"),
        axis.title.x = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20))
#+
#ggtitle("Variation over u")

