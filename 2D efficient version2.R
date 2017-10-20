
set.seed(123113)
x1<-c(0.6276,0.2552,0.1545,0.8286,0.4089,0.5792,0.2365,0.4967,0.6811)
y1<-c(0.2772,0.5599,0.0257,0.0545,0.8235,0.0441,0.1227,0.3219,0.1024)
data<-cbind(x1,y1)
data<-data[order(data[,1]),]
dataT<-data[1:3,]
data<-replicate(2,rnorm(6))
data<-data[order(data[,1]),]
projFun<-Vectorize(function(theta,x1,y1){
  return(sum(c(cos(theta),sin(theta))*c(x1,y1)))
}
)


projFun2<-Vectorize(function(theta,x){
  return(sum(c(cos(theta),sin(theta))*x))
}
)
cols<-order(data[,1])
#quartz()
curve(projFun(x,data[1,1],data[1,2]),from=0,to=pi,ylim=c(-3,3),
      lwd=2,col=2,xlab="Theta",ylab="")
for(i in 2:length(data[,1])){ 
  curve(projFun(x,data[i,1],data[i,2]),from=0,to=pi,add=T,col=1+i,lwd=3)
}
legend(0.25,1.25,sapply(1:(length(data[,1])),toString),lty=1,col=1:5+1)
#returns the certificate related to two curves
getCert<-function(a,b,min=F){
  
  
  if(a[3]!=b[3]){
    t<-atan((b[1]-a[1])/(a[2]-b[2]))
    if(t<0)
      t<-t+pi
    #which is max(min if min is true)
    cert<-list("expiry"=t,"order"=c(a[3],b[3]),"min"=min)
    if(a[1]<b[1])
      cert$order<-cert$order[c(2,1)]
    if(min)
      cert$order<-cert$order[c(2,1)]
  }
  
  else
    cert<-list("order"=rep(a[3],2),"expiry"=10,"min"=min)
  
  return(cert)
}

#adds and event to the queue
enqueue<-function(queue,event){
  
  if(is.null(event))
    return(queue)
  
  else if(is.null(queue)){
    return(list(event))
  }
  
  else{
    i<-1
    condition<-F
    n<-length(queue)
    
    while(!condition&&i<=n){
      if(isEqualEvent(event$cert,queue[[i]]$cert)){
        queue[[i]]<-event
        condition=T
        break
      }
      
      condition<-event$cert$expiry<queue[[i]]$cert$expiry
      
      if(condition){
        if(n>1)
          queue[i:(n+1)]<-append(list(event),queue[i:n])
        else
          queue<-append(list(event),queue[1])
        
      }
      i<-i+1
    }
    
    
    if(i==(n+1)&&!condition)
      queue[(n+1)]<-list(event)
    
    return(queue)
  }
}

isEqualEvent<-function(e1,e2){
  
  if(identical(e1$expiry,e2$expiry))
    return(sum(duplicated(c(e1$order,e2$order)))==2)
  else
    return(FALSE)
}

#returns the current champ then the non champ
getCurrOrder<-function(time,cert){
  
  if(cert$expiry>time)
    return(cert$order)
  else
    return(cert$order[c(2,1)])
  
}

getParent<-function(time,cert){
  
  if(cert$expiry>time)
    return(cert$order[1])
  else
    return(cert$order[2])
  
}

getIndex<-function(pos){
  
  ind<-1
  i<-2
  while(i<=length(pos)){
    ind<-ind*2+pos[i]%/%2
    i<-i+1
  }
  return(ind)
}
#vals is a matrix of values at starting time on column 1
#and the line number (letter in examples) in column 2
createTreeAndQ<-function(vals,tree=list(),queue=NULL,data,pos=as.vector(0),min=F){
  
  if(is.null(dim(vals))){
    tree[[getIndex(pos)]]<-list("cert"=list("order"=rep(vals[2],2),"expiry"=10),"parentVal"=vals[1],"pos"=pos)
    return(list("tree"=tree,"queue"=queue))
  }
  else if(length(vals[,1])>2){
    
    n<-floor(length(vals[,1])/2)
    tree<-createTreeAndQ(vals[1:n,],tree,queue,data,pos=c(pos,1),min)
    queue<-tree$queue
    tree<-tree$tree
    tree<-createTreeAndQ(vals[(n+1):length(vals[,1]),],tree,queue,data,pos=c(pos,2),min)
    queue<-tree$queue
    tree<-tree$tree
    newVals<-cbind(c(tree[[getIndex(c(pos,1))]]$parentVal,tree[[getIndex(c(pos,2))]]$parentVal),c(tree[[getIndex(c(pos,1))]]$cert$order[1],tree[[getIndex(c(pos,2))]]$cert$order[1]))
    tree<-createTreeAndQ(newVals,tree,queue,data,pos,min)
    queue<-tree$queue
    tree<-tree$tree
    
    if(length(pos)>1)
      return(list("tree"=tree,"queue"=queue))
    else{
      #tree2<-list()
      #for(i in 1:length(tree))
      #t#ree2[getIndex(tree[[i]]$pos)]<-tree[i]
      return(list("tree"=tree,"queue"=queue))
    }
    
  }
  else{
    cert<-getCert(data[vals[1,2],],data[vals[2,2],],min)
    
    tree[[getIndex(pos)]]<-list("cert"=cert,"parentVal"=data[cert$order[1],1],"pos"=pos)
    return(list("tree"=tree,"queue"=enqueue(queue,list("cert"=cert,"pos"=pos))))
    
  }
  
  
}

#essentially dequeue
getNextEventEven<-function(kdTree){
  
  event<-kdTree$queue[[1]]
  if(is.null(kdTree$queue))
    return(list("tree"=kdTree,"event"=event))
  else if(event$cert$expiry<=pi){
    
    min<-event$cert$min
    
    if(min){
      tree<-kdTree$treeMin
      tree2<-kdTree$treeMax
    }
    else {
      tree<-kdTree$treeMax
      tree2<-kdTree$treeMin
    }
    #dequeue
    if(length(kdTree$queue)>1)
      kdTree$queue<-kdTree$queue[2:length(kdTree$queue)]
    else
      kdTree$queue<-NULL
    
    #update tree
    #get spot on tree (2 is right, left is 1)
    posit<-event$pos
    #not root
    if(posit[1]!=-19){
      time<-event$cert$expiry
      index<-getIndex(posit)
      #skip this cert if the order has changed
      skip<-!isEqualEvent(event$cert,tree[[index]]$cert)
    }
    #root
    else{
      time<-event$cert$expiry
      #do not skip event if the roots have not changed
      minRoot<-getParent(time,kdTree$treeMin[[1]]$cert)
      maxRoot<-getParent(time,kdTree$treeMax[[1]]$cert)
      skip<-!sum(sort(event$cert$order)==sort(c(minRoot,maxRoot)))==2
#      skip=T
    }
  }
  else{
    skip=F
    event=NULL
  }
  while(skip){
    
    if(is.null(kdTree$queue))
      break
    
    event<-kdTree$queue[[1]]
    if(event$cert$expiry<=pi){
      min<-event$cert$min
      
      if(min){
        tree<-kdTree$treeMin
        tree2<-kdTree$treeMax
      }
      else {
        tree<-kdTree$treeMax
        tree2<-kdTree$treeMin
      }
      #dequeue
      if(length(kdTree$queue)>1)
        kdTree$queue<-kdTree$queue[2:length(kdTree$queue)]
      else
        kdTree$queue<-NULL
      
      #update tree
      #get spot on tree (2 is right, left is 1)
      posit<-event$pos
      #not root
      if(posit[1]!=-19){
        time<-event$cert$expiry
        index<-getIndex(posit)
        #skip this cert if the order has changed
        skip<-!isEqualEvent(event$cert,tree[[index]]$cert)
      }
      #root
      else{
        time<-event$cert$expiry
        #do not skip event if the roots have not changed
        minRoot<-getParent(time,kdTree$treeMin[[1]]$cert)
        maxRoot<-getParent(time,kdTree$treeMax[[1]]$cert)
        skip<-!sum(sort(event$cert$order)==sort(c(minRoot,maxRoot)))==2
        #      skip=T
      }
      
    }
    else{
      event=NULL
      kdTree$queue=NULL
      break
    }
  }
  if(skip)
    event=NULL
  
  return(list("tree"=kdTree,"event"=event))
}

#processes next event in queue
processEventEven<-function(kdTree,data){
  
  event<-kdTree$event
  time<-event$cert$expiry
  kdTree<-kdTree$tree
  
  if(!is.null(event)){
    #root switch
    if(event$pos[1]==-19){
      minRoot<-getParent(time,kdTree$treeMin[[1]]$cert)
      maxRoot<-getParent(time,kdTree$treeMax[[1]]$cert)
      #minRoot<-kdTree$treeMin[[1]]$cert$order[1]
      #maxRoot<-kdTree$treeMax[[1]]$cert$order[1]
      kdTree<-trickleDown(kdTree,maxRoot,minRoot,data,T,time)
      kdTree<-trickleDown(kdTree,minRoot,maxRoot,data,F,time)
    }
    else{
      min<-event$cert$min
      
      if(min)
        tree<-kdTree$treeMin
      else
        tree<-kdTree$treeMax
      index<-getIndex(event$pos)
      
      
      #swap current nodes(dont actually need to do anything)
      oldW<-tree[[index]]$cert$order[1]
      newW<-tree[[index]]$cert$order[2]
      continue2<-index>1
      if(continue2){
        upLevel<-(index-event$pos[length(event$pos)]%/%2)/2
        
        compare<-tree[[upLevel]]
        #curr max then min
        compareCurr<-getCurrOrder(time,compare$cert)
        # if(compare$cert$min!=min)
        #   compare$cert$order<-compare$cert$order[c(2,1)]
        continue<-compareCurr[1]==oldW
        if(continue)
          newCert<-getCert(data[newW,],data[compareCurr[2],],min)
        else
          newCert<-getCert(data[newW,],data[compareCurr[1],],min)
        tree[[upLevel]]$cert<-newCert
        expired<-newCert$expiry<=time
        if(!expired)
          kdTree$queue<-enqueue(kdTree$queue,list("cert"=newCert,"pos"=compare$pos))
        continue2<-length(unlist(compare$pos))>1
        
        
        #compare to neighboring child and enqueue new cert if the time is greater than current time
        #if larger, repeat by swapping with parent and so on.
        while(continue2&&continue){
          
          upLevel<-(upLevel-compare$pos[length(compare$pos)]%/%2)/2
          compare<-tree[[upLevel]]
          #curr max then min
          compareCurr<-getCurrOrder(time,compare$cert)
          continue<-compareCurr[1]==oldW
          if(continue)
            newCert<-getCert(data[newW,],data[compareCurr[2],],min)
          else
            newCert<-getCert(data[newW,],data[compareCurr[1],],min)
          tree[[upLevel]]$cert<-newCert
          expired<-newCert$expiry<=time
          if(!expired)
            kdTree$queue<-enqueue(kdTree$queue,list("cert"=newCert,"pos"=compare$pos,"min"=min))
          continue2<-length(unlist(compare$pos))>1
          
        }
        #if median has changed, trickle down other tree
        
        
      }
      else{
        continue=T
      }
      
      if(min)
        kdTree$treeMin<-tree
      else
        kdTree$treeMax<-tree
      
      
      #if root replaced
     if(!continue2&&continue){
       minRoot<-getParent(time,kdTree$treeMin[[1]]$cert)
       maxRoot<-getParent(time,kdTree$treeMax[[1]]$cert)
       cert1<-getCert(data[minRoot,],data[maxRoot,],F)
       event<-list("cert"=cert1,"pos"=-19)
       if(cert1$expiry>time)
         kdTree$queue<-enqueue(kdTree$queue,event)
#       kdTree<-trickleDown(kdTree,newW,oldW,data,!min,time)
     }
      
    }
  }
  return(kdTree)
}

#trickled a val down the tree
trickleDown<-function(kdTree,newW,oldW,data,min,time){
  
  if(min)
    tree<-kdTree$treeMin
  else
    tree<-kdTree$treeMax
  
  
  curr<-getCurrOrder(time,tree[[1]]$cert)[2]
  newCert<-getCert(data[newW,],data[curr,],min)
  tree[[1]]$cert<-newCert
  expired<-newCert$expiry<time
  if(!expired)
    kdTree$queue<-enqueue(kdTree$queue,list("cert"=newCert,"pos"=tree[[1]]$pos))
  if(any(tree[[2]]$cert$order==oldW))
    nextChild<-2
  else
    nextChild<-3
  
  while(!is.null(tree[[nextChild]])){
    
    if(tree[[nextChild]]$cert$expiry!=10){
      curr<-getCurrOrder(time,tree[[nextChild]]$cert)[2]
      newCert<-getCert(data[newW,],data[curr,],min)
      tree[[nextChild]]$cert<-newCert
      expired<-newCert$expiry<time
      if(!expired)
        kdTree$queue<-enqueue(kdTree$queue,list("cert"=newCert,"pos"=tree[[nextChild]]$pos))
      
      if(length(tree)>=2*nextChild+1){
        if(any(tree[[2*nextChild]]$cert$order==oldW))
          nextChild<-2*nextChild
        else
          nextChild<-2*nextChild+1
      }
      else if(length(tree)>=2*nextChild){
        if(any(tree[[2*nextChild]]$cert$order==oldW))
          nextChild<-2*nextChild
      }
      else 
        break
    }
    else{
      tree[[nextChild]]$cert$order=c(newW,newW);
      break
    }
  }
  
  if(min)
    kdTree$treeMin<-tree
  else
    kdTree$treeMax<-tree
  
  return(kdTree)
  
}

#combines 2 event queues
combineQs<-function(q1,q2){
  
  for(i in 1:length(q1)){
    
    q2<-enqueue(q2,q1[[i]])
  }
  return(q2)
}

calcMed2DEffEven<-function(data,med=F){
  
  data<-data[order(data[,1]),]
  n<-length(data[,1])
  # even<-n%%2==0
  data2<-cbind(data,1:n)

    split<-n/2
    kdMax<-createTreeAndQ(vals=data2[1:split,c(1,3)],data=data2)
    kdMin<-createTreeAndQ(vals=data2[(split+1):n,c(1,3)],data=data2,min=T)
    cert1<-getCert(data2[kdMin$tree[[1]]$cert$order[1],],data2[kdMax$tree[[1]]$cert$order[1],],F)
    #-19 is the position
    event<-list("cert"=cert1,"pos"=-19)
    queue<-combineQs(kdMin$queue,kdMax$queue)
    queue<-enqueue(queue,event)
    kd<-list("treeMin"=kdMin$tree,"treeMax"=kdMax$tree,"queue"=queue)
    
    time1<-0
    kd<-getNextEvent(kd)
    time2<-kd$event$cert$expiry
    champMin<-kd$tree$treeMin[[1]]$cert$order[1]
    champMax<-kd$tree$treeMax[[1]]$cert$order[1]
    medians<-matrix(c(champMin,champMax,time1,time2),nrow=1)
    
    # print("queue")
    # printQ(kd$tree$queue)
    # printT(kd$tree$treeMin)
    # printT(kd$tree$treeMax)
    # print(kd$event$cert)
    # printQ(kd$queue)
    # printT(kd$treeMin)
    # printT(kd$treeMax)
    ##while queue is not empty
    while(!is.null(kd$event)){
   # for(i in 1:4){
      #process next item in queue
      
      kd<-processEventEven(kd,data2)
      
      #record champion
      champMin<-getParent(time2,kd$treeMin[[1]]$cert)
      champMax<-getParent(time2,kd$treeMax[[1]]$cert)
      time1<-time2
      kd<-getNextEventEven(kd)
      time2<-kd$event$cert$expiry
      if(is.null(time2))
        time2<-pi
      
        if(champMax!=medians[nrow(medians),2]||champMin!=medians[nrow(medians),1])
          medians<-rbind(medians,c(champMin,champMax,time1,time2))
        else
          medians[nrow(medians),4]<-medians[nrow(medians),4]+time2-time1
      
      
    }
    medians[nrow(medians),4]<-pi
    w<-medians[,4]-medians[,3]
    w<-w/pi
    #  print(w)
    #  print(sum(w))
    # print(medians)
    if(med)
      return(medians)
    else
      return(w%*%(data[medians[,1],]+data[medians[,2],])/2)
    
  }


 proMed<-calcMed2DEffEven(data)
 medians<-calcMed2DEffEven(data,T)
 
#
 
 curve(projFun(x,data[1,1],data[1,2]),from=0,to=pi,ylim=c(-3,3),
       lwd=2,col=2,xlab="Theta",ylab="")
for(i in 2:length(data[,1])){ 
   curve(projFun(x,data[i,1],data[i,2]),from=0,to=pi,add=T,col=1+i,lwd=3)
 }
for(i in 1:length(medians[,1])){
  curve(projFun(x,data[medians[i,1],1],data[medians[i,1],2]),from=medians[i,3],to=medians[i,4],add=T,col="orange",lty=1,lwd=4)
}

 for(i in 1:length(medians[,1])){
   curve(projFun(x,data[medians[i,2],1],data[medians[i,2],2]),from=medians[i,3],to=medians[i,4],add=T,col="black",lty=1,lwd=4)
 } 
# 
# 
# # > data3<-replicate(2,rnorm(2000))
# # > system.time(calcMed2DEff(data3))
# # user  system elapsed 
# # 388.16    0.60  556.41 
# # > data4<-replicate(2,rnorm(200))
# # > system.time(calcMed2DEff(data4))
# # user  system elapsed 
# # 2.88    0.00    3.17
# printQ<-function(q){
#   for(i in 1:length(q)){
#     print(paste(toString(q[[i]]$cert$order),q[[i]]$cert$expiry,sep=" "))
#   }
# }
# 
# printT<-function(tree){
#   
#   for(i in 1:length(tree)){
#     print(paste(toString(tree[[i]]$cert$order),sep=" "))
#   }
#   
# }
# 
# # data3<-replicate(2,rnorm(2001))
# data4<-replicate(2,rnorm(201))
# 
# dataSizes<-c(2000)+1
# dataSizes2<-dataSizes^(4/3)
# plot(dataSizes,dataSizes2,type='l',xlab="n",ylab="vertices in median level")
# vertices<-matrix(nrow=3,ncol=length(dataSizes))
# for(j in 1:3){
#   for(i in 1:length(dataSizes))
#     vertices[j,i]<-length(calcMed2DEff(replicate(2,rnorm(dataSizes[i],sd=10)),T)[,1])
# }
# 
# maxV<-apply(vertices,2,max)
# maxV/2
# 
# lnV<-log(dataSizes)
# 
# mn<-lm(vertices~dataSizes)
# 
# lmn<-lm(log(apply(vertices,2,max))~lnV)
# 
# 
# 
# plot(dataSizes,dataSizes*lmn2$coefficients[2]+lmn2$coefficients[1],col="blue",xlab="n",ylab="vertices in median level",type='l') 
# points(dataSizes,apply(vertices,2,max))
# points(dataSizes,dataSizes2+2*(sqrt(12+dataSizes^2)-5)+3,type="l")
# 
# system.time(projWeights(data4))
# #n<-2001
# # > system.time(calcMed2DEff(data4))
# # user  system elapsed 
# # 128.654   0.188 128.826 
# # > system.time(projWeights(data4))
# # user  system elapsed 
# # 210.350  20.934 231.318 
# 
# 
# # > system.time(calcMed2DEff(data4))
# # user  system elapsed 
# # 1.179   0.004   1.184 
# # > system.time(ExPDdatanew(data4))
# # 
# # Timing stopped at: 881.188 3.261 884.45 