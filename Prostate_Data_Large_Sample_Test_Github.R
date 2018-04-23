set.seed(23623)

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

data<-tab2
mad<-function(x){median(abs(x-median(x)))}
N<-10000
d<-10
normalize<-function(x){
  return(x/sqrt(sum(x^2)))
}
x1<-replicate(d,rnorm(N))
x1<-apply(x1,MARGIN=1,normalize)


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

findUnRank<-function(proj,even){
  
  n<-length(proj)
  
  if(even)
    med<-c(getMidOrderStat(proj,n/2),getMidOrderStat(proj,n/2+1))
  
  else
    med<-which(proj==median(proj))
  
  return(med)
}

singleRanks<-function(projections,even){
  
  
  
  if(!even){
    med<-median(projections)
    ranks<-projections
    ranks[projections<med]<- -1
    ranks[projections>med]<- 1
    ranks[projections==med]<-0
  }
  #even 
  
  else{
    n<-length(projections)
    p1<-n/2
    p2<-p1+1
    med1<-quantile(projections,c(p1,p2)/n,type=1)
    ranks<-projections
    ###0.5 for upper med and -0.5 for lower median.
    ranks[projections<med1[1]]<- -1
    ranks[projections>med1[2]]<- 1
    ranks[projections==med1[1]]<--0.5
    ranks[projections==med1[2]]<-0.5
  }
  return(ranks)#returns ranks of the data if below above or is the median
}
getProjection<-function(x,data){
  ##all the points projected onto u
  return(data%*%t(t(x)))  
}
getEstimates<-function(data){
  
  n<-nrow(data)
  even=n%%2==0
  
  ###rows are observations
  projections<-apply(x1,MARGIN=2,getProjection,data=data)
  
  ###matrix of ranks of data at each u.
  univ.ranks<-apply(projections,MARGIN=2,singleRanks,even=even)
  #rows are observations
  
  #for each row get how many times it was the median
  numMedd<-function(roww){
    numMed<-N-sum(roww==1)-sum(roww==-1)
    
    return(numMed)
  }
  
  stats<-apply(univ.ranks,1,numMedd)
  w<-stats/sum(stats)
  
  
  medians<-apply(projections,2,median)
  bw<-1.06*apply(projections,2,mad)*n^(-0.2)
  getHeight<-Vectorize(function(ind){return(weighted.mean(dnorm((medians[ind]-projections[,ind])/bw[ind])/bw[ind],w=w))})
  heights<-sapply(1:N,getHeight)
  
  
  # getHeight2<-Vectorize(function(ind){return(mean(dnorm((medians[ind]-projections[,ind])/bw[ind])/bw[ind]))})
  # heights2<-sapply(1:N,getHeight2)
  # this is not weighted
  #get indicators
  univ.ranks2<-univ.ranks
  univ.ranks2[univ.ranks>=0.5]=0
  univ.ranks2[univ.ranks<0]=1
  #FRM<-function(roww){return(matrix(roww,ncol=1)%*%matrix(roww,nrow=1))}
  
  
  sigma<-matrix(0,nrow=d,ncol=d) 
  for(j in 1:(N-1)){
    for(k in (j+1):N){
      sigma<-sigma+(mean(univ.ranks2[,j]*univ.ranks2[,k])-0.25)*matrix(x1[,j],ncol=1)%*%matrix(x1[,k],nrow=1)/(heights[k]*heights[j])
    }
  }
  
  sigma<-sigma*2 
  for(k in 1:N){
    sigma<-sigma+(0.25)*matrix(x1[,k],ncol=1)%*%matrix(x1[,k],nrow=1)/(heights[k]^2)
  }
  
  sigma<-sigma/N^2
  pm<-sweep(data,MARGIN=1,w,'*')
  pm<-apply(pm,2,sum)
  
  return(list("pm"=pm,"sigma"=sigma))
}



calculateTestStat<-function(data){
  m<-N
  d<-ncol(data)
  
  
  
  
  pm1<-getEstimates(data[1:50,])
  
  pm2<-getEstimates(data[51:102,])
  
  sigma<-(pm1$sigma*(50-1)+pm2$sigma*51)/100
  
  ts<-matrix(pm1$pm-pm2$pm,nrow=1)%*%solve(sigma)%*%matrix(pm1$pm-pm2$pm,ncol=1)
  #  print("pval")
  # print(pchisq(ts,d-2,lower.tail=F))
  return(ts)
}

tss3<-calculateTestStat(tab2)
#f 10,91
n1<-50
n2<-52
fStat<-tss3*(n1+n2-d-1)/(d*(n1+n2-2))
print(pf(fStat,10,91,lower.tail=F))
