library(MASS)
library(parallel)
library(doParallel)
tab<-biopsy


set.seed(23623)#for unit vectors


# Calculate the number of cores
no_cores <- detectCores() - 1


#calculate PM,  tested
projMC<-function(data){
  
  
  
  singlePM<-function(x,data){
    even=length(data)%%2==0
    #run the projmed monte carlo script
    getMidOrderStat<-function(x,pos){
      for(i in 1:length(x)){
        if(sum(x<x[i])==(pos-1)){
          ind<-i
          break
        }
      }
      return(i)
    }
    findUnMed2<-function(proj,even){
      
      n<-length(proj)
      
      if(even)
        med<-c(getMidOrderStat(proj,n/2),getMidOrderStat(proj,n/2+1))
      
      else
        med<-which(proj==median(proj))
      
      return(med)
    }
    
    ##all the points projected onto u
    projections<-x[2]*data%*%t(t(x))
    ##median is median length
    temp<-findUnMed2(projections,even)
    
    return(temp)#returns index of points
  }
  
  getWeights<-function(data,x1){
    
    c2=apply(x1,MARGIN=2,singlePM,data=as.matrix(data))
    
    
    identify<-Vectorize(function(x,c2){
      
      val<-sum(sapply(c2,identical,y=x))
      
      return(val)
    },vectorize.args = 'x')
    
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    w<-foreach(i=1:length(data[,1])) %dopar% identify(i,c2=c2)
    stopCluster(cl)
    w<-unlist(w)
    w<-w/length(c2)
    
    
    
    return(w)
  }
  
  
  
  d<-ncol(data)
  
  w<-getWeights(as.matrix(data),x1)
  pm<-matrix(w,nrow=1)%*%as.matrix(data)
  
  return(list("pm"=pm,"w"=w))
}



dim(tab)
d<-dim(tab)[2]-2
tab2<-tab[,-1]
names(tab2)[d+1]="class"

remove(tab)

#generate unit vectors
N<-10000
m<-N

normalize<-function(x){
  return(x/sqrt(sum(x^2)))
}
x1<-replicate(d,rnorm(N))

x1<-apply(x1,MARGIN=1,normalize)


#start calculating test statistic

data<-tab2
remove(tab2)
m<-N

#estimate variance
estVarr<-function(data,w,m,x1){
  
  data<-as.matrix(data)
  
  #dot products, row is observation, column is unit vector
  dotProds<-data%*%x1
  
  #medians Fnu inverse at 1/2
  cl <- makeCluster(no_cores)
  medians<-parApply(cl,dotProds,MARGIN=2,median)
  
  
  #clusterExport(cl,"dotProds")
  #  Nxn, indicates if below or above median for each u and x
  indicators<-parApply(cl,dotProds,1,"<=",y=medians)
  
  
  
  n<-nrow(data)
  
  #bandwidths for the density estimate
  bw<-1.06*apply(dotProds,2,mad)*n^(-0.2)
  
  
  #height of density estimate at median
  height<-Vectorize(function(i,medians,dotProds,bw,w){
    weighted.mean(dnorm((medians[i]-dotProds[,i])/bw[i])/bw[i],w)
  },vectorize.args = 'i')
  
  
  #heights of density estimates at medians
  heights<-parSapply(cl,1:m,height,medians=medians,dotProds=dotProds,bw=bw,w=w)
  
  
  
  
  #terms in the sum, i doesn't equal j
  terms<-Vectorize(function(i,j,indicators,x1,heights){
    #pijs
    pij<-mean(mapply('&&',indicators[i,],indicators[j,]))
    #uu'
    uu<-matrix(x1[,i],ncol=1)%*%matrix(x1[,j],nrow=1)
    #heights
    hh<-heights[j]*heights[i]
    #term in sum
    sums<-(pij-.25)*uu/hh
    return(sums)
  },vectorize.args = 'j')
  
  #for using do parrallel
  terms2<-function(i,indicators,x1,heights,m){
    val<-rowSums(terms(i,(i+1):m,indicators,x1,heights))
    return(val)
  }
  
  
  #clearing some memory
  gc()
  
  #calc sum
  registerDoParallel(cl)
  sums<-2*Reduce(`+`,foreach(i=1:(m-1)) %dopar% terms2(i,indicators,x1,heights,m))
  
  #i=j
  terms3<-Vectorize(function(i,x1,heights){
    
    #uu'
    uu<-matrix(x1[,i],ncol=1)%*%matrix(x1[,i],nrow=1)
    #heights
    hh<-heights[i]^2
    #term in sum
    sums<-(.25)*uu/hh
    return(sums)
  },vectorize.args = 'i')
  
  iequalsj<-Reduce(`+`,foreach(i=1:m) %dopar% terms3(i,x1,heights))
  
  #  make a matrix
  sigma<-(d/m)^2*matrix(sums+c(iequalsj),ncol=d)
  
  return(sigma)
}
data1<-subset(data,class=="malignant")[,-(d+1)]
data2<-subset(data,class=="benign")[,-(d+1)]
data1<-as.matrix(data1[complete.cases(data1),])
data2<-as.matrix(data2[complete.cases(data2),])




pm1<-projMC(data1)

pm2<-projMC(data2)

n1<-nrow(data1)
n2<-nrow(data2)

v1<-estVarr(data1,pm1$w,m,x1)
v2<-estVarr(data2,pm2$w,m,x1)
#sigma<-(v1*(n1-1)+v2*(n2-1))/(n1+n2-2)


chis<-(pm1$pm-pm2$pm)%*%solve(v1/n1+v2/n2)%*%t(pm1$pm-pm2$pm)
print(pchisq(chis,d,lower.tail=F))
#print(xtable(v2*100), floating=FALSE, tabular.environment="bmatrix", hline.after=NULL, include.rownames=FALSE, include.colnames=FALSE)

save(ts,n1,n2,fStat,sigma,v1,v2,pm1,pm2,chis,file="test_stat.RData")
