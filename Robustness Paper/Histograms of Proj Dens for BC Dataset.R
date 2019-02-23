#median avg deviation
mad<-function(x){return(median(abs(x-median(x))))}

library(latex2exp)
library(ggthemes)
library(MASS)
library(ggplot2)
#data
tab<-biopsy

d=9
set.seed(23623)#for unit vectors

N<-10000
m=N

normalize<-function(x){
  return(x/sqrt(sum(x^2)))
}

#x1 is the unit vectors, se Meuller
x1<-replicate(d,rnorm(N))
x1<-apply(x1,MARGIN=1,normalize)


dim(tab)
d<-dim(tab)[2]-2
data<-tab[,-1]
names(data)[d+1]="class"
remove(tab)

data1<-subset(data,class=="malignant")[,-(d+1)]
data2<-subset(data,class=="benign")[,-(d+1)]
data1<-as.matrix(data1[complete.cases(data1),])
data2<-as.matrix(data2[complete.cases(data2),])


proj1<-as.matrix(data1)%*%x1
proj2<-as.matrix(data2)%*%x1

spreads1<-apply(proj1,2,mad)
spreads2<-apply(proj2,2,mad)

# hist(spreads1)
# hist(spreads2)

spreads<-data.frame(c(rep("M",N),rep("B",N)),c(spreads1,spreads2))


#calculating heights of densities
getHeights<-function(data,w){
data<-as.matrix(data)

#dot products, row is observation, column is unit vector
dotProds<-data%*%x1


medians<-apply(dotProds,MARGIN=2,median)
n<-nrow(data)

#bandwidths for the density estimate
bw<-1.06*apply(dotProds,2,mad)*n^(-0.2)


#height of density estimate at median
height<-Vectorize(function(i,medians,dotProds,bw,w){
  weighted.mean(dnorm((medians[i]-dotProds[,i])/bw[i])/bw[i],w)
},vectorize.args = 'i')


#heights of density estimates at medians
heights<-sapply(1:m,height,medians=medians,dotProds=dotProds,bw=bw,w=w)
return(heights)
}

#pm weights are loaded from the BCTest Script
heights1<-getHeights(data1,w=pm1$w)
heights2<-getHeights(data2,w=pm2$w)

# hist(1/heights1)
# hist(1/heights2)


va<-data.frame(c(rep("M",N),rep("B",N)),c(spreads1,spreads2),1/c(heights1,heights2))
names(va)<-c("Class","Variance of Spread","InvDH")

  
#8x8
ggplot(data=subset(va,Class=="B"),aes(x=InvDH))+
  geom_histogram(binwidth=.5,color="black")+
  xlab(TeX('$\\frac{1}{\\hat{\\mathit{f}}_u(\\xi_u)}$'))+
  scale_fill_economist()+
  theme_economist()
#8x8
ggplot(data=subset(va,Class=="M"),aes(x=InvDH))+
  geom_histogram(binwidth=.5,color="black")+
  xlab(TeX('$\\frac{1}{\\hat{\\mathit{f}}_u(\\xi_u)}$'))+
  scale_fill_economist()+
  theme_economist()
