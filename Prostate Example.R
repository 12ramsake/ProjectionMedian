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

projMC<-function(data,N=10000,method=1){
  
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



set.seed(23623)
 #setwd("C:/Users/12RAM/OneDrive/Documents/research/Projection Median/Kelly/Asymptotics Projection Median/R code")
tab<-read.csv("prostmat.csv",header = F)
dim(tab)
tab2<-apply((tab[-1,]),2,as.numeric)
#tab3<-apply(tab2,2,log)
dim(tab2)
tab2<-t(tab2)[,1:10]
#normalize by mad to remove scale dependence
MAD<-function(data){
  MADS<-c()
  for(i in 1:ncol(data))
    MADS<-c(MADS,median(abs(data[,i]-median(data[,i]))))
  return(MADS)
}
mads<-MAD(tab2)
for(i in 1:10){
  tab2[,i]<-tab2[,i]/mads[i]
}

calculateTestStat<-function(data,m=1000){
  
  d<-ncol(data)
  estVarr<-function(data){
  unit.vectors<-replicate(d,rnorm(m))
  unit.vectors<-apply(unit.vectors,MARGIN=1,normalize)
  com<-combn(m,2)
  #dot products, row is observation, column is unit vector
  dotProds<-data%*%matrix(unit.vectors,ncol=m)
  #which medians Fnu inverse at 1/2
  medians<-apply(unit.vectors,MARGIN=2,median)
  #density estimates
  density.est<-apply(dotProds,2,density)
  heights<-NULL
  #unit by obvs
  indicators<-apply(dotProds,1,"<=",y=medians)
  n<-nrow(data)
  for(i in 1:m){
    bw<-1.06*sd(dotProds[,i])*n^(-0.2)
    heights<-c(heights,mean(dnorm((medians[i]-dotProds[,i])/bw)/bw))
  }
  sum1<-matrix(0,nrow=d,ncol=d)
  sum2<-matrix(0,nrow=d,ncol=d)
  for(i in 1:ncol(com)){
    pij<-mean(mapply('&&',indicators[com[1,i],],indicators[com[2,i],]))
    sum1<-sum1+(pij/(heights[com[1,i]]*heights[com[2,i]]))*matrix(unit.vectors[,com[1,i]],ncol=1)%*%matrix(unit.vectors[,com[2,i]],nrow=1)
    sum2<-sum2+(0.25/(heights[com[1,i]]*heights[com[2,i]]))*matrix(unit.vectors[,com[1,i]],ncol=1)%*%matrix(unit.vectors[,com[2,i]],nrow=1)
  }
  for(i in 1:m){
    sum2<-sum2+(0.25/(heights[i]^2))*matrix(unit.vectors[,i],ncol=1)%*%matrix(unit.vectors[,i],nrow=1)
  }
  sum1<-sum1/(m^2-m)
  sum2<-sum2/m^2
  
  sigma<-sum1-sum2
  return(sigma)
  }
  
  sigma<-(estVarr(data[1:50,])*(50-1)+estVarr(data[51:102,])*51)/100
  
  set.seed(23623)
  pm1<-projMC(data[1:50,],N=m)
  set.seed(23623)
  pm2<-projMC(data[51:102,],N=m)
  
  ts<-t(pm1-pm2)%*%solve(sigma)%*%t(t(pm1-pm2))
#  print("pval")
 # print(pchisq(ts,d-2,lower.tail=F))
  return(ts)
}

tss2<-calculateTestStat(tab2,m=10000)
#f 10,91
fStat<-tss2*(50*52/102)*(102-10-1)/1000
pf(fStat,10,91,lower.tail=F)
save(tss,file="testStat.RData")