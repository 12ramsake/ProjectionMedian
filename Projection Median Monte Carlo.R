
projectionMedianMC<-function(data,N=10000,method=1){
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
  
  singlePM<-function(x,data,even){
    
    ##all the points projected onto u
    projections<-x[2]*data%*%t(t(x))
    ##median is median length
    temp<-findUnMed2(projections,even)
    
    return(temp)#returns index of points
  }
  
  normalize<-function(x){
    return(x/sqrt(sum(x^2)))
  }
  
  
  getWeights<-function(data,x1){
    
    
    c2<-apply(x1,MARGIN=2,singlePM,data=data,even=nrow(data)%%2==0)
    
    identify<-Vectorize(function(x){
      return(sum(sapply(c2,identical,y=x)))
    })
    
    w<-sapply(1:length(data[,1]),identify)/length(c2)
    return(w)
  }
  
  d<-ncol(data)
  
  x1<-replicate(d,rnorm(N))
  x1<-apply(x1,MARGIN=1,normalize)
  if(method==1){
    c2<-data[apply(x1,MARGIN=2,singlePM,data=data,even=nrow(data)%%2==0),]
    pm<-apply(c2,MARGIN=2,mean)
  }
  else{
    w<-getWeights(data,x1)
    pm<-matrix(w,nrow=1)%*%data
  }
  return(pm)
}

