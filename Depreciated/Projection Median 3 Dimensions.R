library(rgl)




projectionMedian3D<-function(data1){
  fillSphTriangle2<-function(verts,color){
    verts<-rbind(verts[[1]],verts[[2]],verts[[3]])
    # triangles3d(verts[,1],verts[,2],verts[,3],col=color)
    
    m<-normalize(apply(verts,2,mean))
    a<-0:3/3
    b<-1-a
    points1<-sapply(a,"*",verts[1,])+sapply(b,"*",verts[2,])
    points1<-apply(points1,2,normalize)
    points2<-sapply(a,"*",verts[1,])+sapply(b,"*",verts[3,])
    points2<-apply(points2,2,normalize)
    points3<-sapply(a,"*",verts[2,])+sapply(b,"*",verts[3,])
    points3<-apply(points3,2,normalize)
    points<-cbind(points1,points2[,length(b):1],points3)
    lines3d(points[1,],points[2,],points[3,])
    
    for(i in 1:(length(points[1,])-1)){
      
      k<-i+1
      
      triangles3d(c(points[1,c(i,k)],m[1]),c(points[2,c(i,k)],m[2]),c(points[3,c(i,k)],m[3]),lit=F,col=color)
      triangles3d(-c(points[1,c(i,k)],m[1]),-c(points[2,c(i,k)],m[2]),-c(points[3,c(i,k)],m[3]),lit=F,col=color)
      
    }
    
    
  }
  
  
  point2plane<-function(points){
    planes<-matrix(0,nrow=nrow(points),ncol=4)
    planes[,1]<-points[,1]*2
    planes[,2]<-points[,2]*2
    planes[,3]<--1
    planes[,4]<-(-1)*points[,3]
    return(planes)
  }
  set.seed(2543)
  #set.seed(2542473)
  matx<-matrix(replicate(3,rnorm(5)),nrow=5)
  # matx<-rbind(c(3,4.1,1),
  #             c(.2,6,1),
  #             c(2,1,0.2),
  #             c(1,3,-4.1),
  #             c(-1,3,5))+matrix(replicate(3,rnorm(5)),nrow=5)
  matx2<-point2plane(matx)
  #matx<-rbind(c(-1, 4, 1, 3),c(1, -1, 22, 1),c(2, 1, -2, -18))
  #print(getMedLevel(matx2))
  
  getAngleBtwn<-function(vectors){
    
    a<-vectors[1:3]
    b<-vectors[4:6]
    return(getAngleBtwn2(a,b))
  }
  
  #this function is pulled from https://stackoverflow.com/questions/15162741/what-is-rs-crossproduct-function
  #credit is to user Chip Hogg
  CrossProduct3D <- function(x, y, i=1:3) {
   
    To3D <- function(x) head(c(x, rep(0, 3)), 3)
    x <- To3D(x)
    y <- To3D(y)
    

    Index3D <- function(i) (i - 1) %% 3 + 1

    return (x[Index3D(i + 1)] * y[Index3D(i + 2)] -
              x[Index3D(i + 2)] * y[Index3D(i + 1)])
  }
  
  getAngleBtwn2<-function(a,b){
    value<-sum(a*b) / ( sqrt(sum(a^2)) * sqrt(sum(b^2)) )
    return(acos(sign(value)*min(abs(value),1)))
  }
  #gives coordinates in cartesian from spherical
  
  normalize<-function(x){
    return(x/sqrt(sum(x^2)))
  }
  
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
  
  singlePM<-function(x,data1,even){
    
    ##all the points projected onto u
    projections<-x[2]*data1%*%t(t(x))
    ##median is median length
    temp<-findUnMed2(projections,even)
    
    return(temp)#returns index of points
  }
  
  #takes 3 vertices in spherical coordinates and returns the median in this triangle
  calcMedTri<-function(tri,data1,even){
    # print("calcing median")
    v1<-tri[[1]]
    v2<-tri[[2]]
    v3<-tri[[3]]
    
    mp<-v1/3+v2/3+v3/3
    
    return(singlePM(mp,data1,even))
  }
  
  
  
  getAngle<-function(a,b,c){
    
    value	<-	(cos(a)-cos(b)*cos(c))/(sin(b)*sin(c))
    
    return(acos(sign(value)*min(abs(value),1)))
  }
  
  #takes 3 vertices in spherical coordinates and returns the area of this triangle
  #checked online to test
  calcArea2<-function(tri){
    
    v1<-tri[[1]]
    v2<-tri[[2]]
    v3<-tri[[3]]
    
    angles<-apply(rbind(c(v1,v2),c(v2,v3),c(v1,v3)),1,getAngleBtwn)
    
    angles2<-c(getAngle(angles[1],angles[2],angles[3]),
               getAngle(angles[3],angles[2],angles[1]),
               getAngle(angles[2],angles[1],angles[3]))
    
    return(sum(angles2)-pi)
  }
  
  
  getBoundaryInter<-function(a,b){
    return(normalize(c(1,-a/b,0)))
  }
  
  fillIn<-function(pair,data1){
    planes<-data1[pair[1],]-data1[pair[2],]
    return(getBoundaryInter(planes[1],planes[2]))
  }
  
  # 
  findCritVector<-function(indices,data1,sign){
    edge=!sum(indices<0)==0
    if(!edge){
      points<-data1[indices+1,]
      v1<-points[1,]-points[2,]
      v2<-points[3,]-points[2,]
      
      p<-CrossProduct3D(v1,v2)
      p<-normalize(p)
      if(sign(p[3])!=sign){
        p<--p
      }
    }
    else{
      pair<-sort(indices)[2:3]+1
      p<--fillIn(pair,data1)
      if(sign(p[3])!=sign){
        p<--p
      }
      
    }
    #points<-processOverlap(points,arcNums)
    
    
    return(p)
  }
  
  getPolygon2<-function(vertices,data1,signs){
    
    coords<-NULL
    for(i in 1:length(vertices)){
      vec<-findCritVector(indices=unlist(vertices[[i]]),data1=data1,unlist(signs)[i])
      coords<-rbind(coords,vec)
    }
    
    return(coords)
  }
  
  
  triangulateConvex2<-function(polygon){
    
    #  polygon<-orderVerts2(polygon)
    home<-polygon[1,]
    
    index<-2
    nextV<-polygon[index,]
    nextV2<-polygon[index+1,]
    
    triangles<-list(list(home,nextV,nextV2))
    
    while(index<(nrow(polygon)-1)){
      index<-index+1
      nextV<-polygon[index,]
      nextV2<-polygon[index+1,]
      triangles[length(triangles)+1]<-list(list(home,nextV,nextV2))
      
    }
    
    return(triangles)
    
  }
  
  getArea<-function(tris){
    area<-0
    for(i in 1:(length(tris))){
      #  t1<-tris[[i]]
      #  t1[[2]]<--t1[[2]]
      ##  t2<-tris[[i]]
      # t2[[3]]<--t2[[3]]
      # t3<-tris[[i]]
      # t3[[1]]<--t3[[1]]
      # tmp<-c(calcArea2(tris[[i]]),calcArea2(t1),calcArea2(t2),calcArea2(t3))
      tmp<-calcArea2(tris[[i]])
      #   if(which(rank(tmp)==1)==1){
      #     t4<-tris[[i]]
      #   }
      #   else if(which(rank(tmp)==1)==2){
      #     t4<-t1
      #   }
      #   else if(which(rank(tmp)==1)==3){
      #     t4<-t2
      #   }
      #   else if(which(rank(tmp)==1)==4){
      #     t4<-t3
      #   }
      # #  fillSphTriangle2(t4,colors()[med*10])
      #   tris[[i]]<-t4
      area<-area+tmp
    }
    return(list("area"=area,"tris"=tris))
  }
  plotP<-function(P,col="black",size=8){
    # P<-rbind(P,-P)
    points3d(P[,1],P[,2],P[,3],size=size,col=col)
  }
  isRelevant<-function(cell){
    n<-length(cell)
    rel<-TRUE
    for(i in 1:n){
      rel<-sum(cell[[i]]<0)==0
      if(rel)
        break
    }
    return(rel)
  }
  cellsEqual<-function(cell1,cell2){
    n1<-length(cell1)
    n2<-length(cell2)
    equal<-n1==n2
    for(i in 1:n1){
      if(equal){
        for(j in 1:n2){
          equal=identical(sort(cell1[[i]]),sort(cell2[[j]]))
          if(equal)
            break
        }
      }
      else
        break
    }
    return(equal)
  }
  isIn<-function(vert,queue){
    iN=length(queue)>0
    if(iN){
      for(i in 1:length(queue)){
        #  print(queue[[i]])
        #  print(vert)
        iN=identical(sort(queue[[i]]),sort(unlist(vert)))
        if(iN)
          break
      }
    }
    return(iN)
  }
  reduceCells<-function(cells){
    n<-length(cells)
    
    for(index in 1:n){
      toRm<-NULL  
      m<-length(cells[[index]])
      
      for(i in 1:m){
        if(sum((unlist(cells[[index]][[i]]))<0)>1){
          toRm<-c(toRm,i)
        }
      }
      if(!is.null(toRm))
        cells[[index]]<-cells[[index]][-toRm]
    }
    return(cells)
  }
  
  combineEven<-function(cells1,cells2){
    
    m<-length(cells1)
    for(i in 1:length(cells2)){
      count<-0
      for(j in 1:length(cells2[[i]])){
        count<-count+as.numeric(sum(cells2[[i]][[j]]<0)>0)
        if(count>1)
          break
      }
      if(count>1){
        cells1<-append(cells1,list(cells2[[i]]))
      }
    }
    
    for(i in 1:m){
      count<-0
      for(j in 1:length(cells1[[i]])){
        count<-count+as.numeric(sum(cells1[[i]][[j]]<0)>0)
        if(count>1)
          break
      }
      if(count>1){
        cells2<-append(cells2,list(cells1[[i]]))
      }
    }
    return(list("cells1"=cells1,"cells2"=cells2))
  }
  getBounded<-function(cells1){
    indices<-NULL
    for(i in 1:length(cells1)){
      count<-0
      for(j in 1:length(cells1[[i]])){
        count<-count+as.numeric(sum(cells1[[i]][[j]]<0)==0)
      }
      if(count==length(cells1[[i]]))
        indices<-c(indices,i)
    }
    return(indices)
  }
  adjacent<-function(vert1,vert2){
    combined<-c(unlist(vert1),unlist(vert2))
    return(sum(as.numeric(duplicated(combined)))==2)
  }
  addNextVert<-function(cell,verts,sv,sc){
    sv<-unlist(sv)
    sc<-unlist(sc)
    for(i in 1:length(verts)){
      ad<-adjacent(verts[[i]],cell[[length(cell)]])
      
      if(ad){
        cell<-append(cell,verts[i])
        verts<-verts[-i]
        sc<-c(sc,sv[i])
        sv<-sv[-i]
        break
      }
    }
    return(list(c1=cell,v=verts,sv=list(sv),sc=list(sc)))
  }
  reOrderCell<-function(verts,sv){
    #print(verts)
    cell<-verts[1]
    verts<-verts[-1]
    sc<-sv[1]
    sv<-sv[-1]
    tog<-list(c1=cell,v=verts,sv=list(sv),sc=list(sc))
    while(length(tog$v)>0){
      
      # print(paste0("cell ",tog$c1))
      #  print(paste0("v ",tog$v))
      tog<-addNextVert(tog$c1,tog$v,tog$sv,tog$sc)
    }
    return(tog[c(1,4)])
  }
  
  combineCells<-function(index,cells,rmvd){
    
    
    n<-length(cells)
    m<-length(cells[[index]])
    pair<-NULL
    toRm<-NULL
    for(i in 1:m){
      if(sum((unlist(cells[[index]][[i]]))<0)==1){
        pair<-rbind(pair,sort(cells[[index]][[i]][cells[[index]][[i]]>=0]))
        toRm<-c(toRm,i)
      }
      else if(sum((unlist(cells[[index]][[i]]))<0)>1){
        toRm<-c(toRm,i)
      }
      
    }
    cells[[index]]<-cells[[index]][-toRm]
    matchingIndex<-NULL
    ##here
    # vertex<-sort(unique(as.vector(pair)))
    if((index+1)<=n){
      for(i in (index+1):n){
        currCell<-cells[[i]]
        matching<-rep(0,4)
        for(j in 1:length(currCell)){
          if((sum((currCell[[j]])<0)==0)){
            matching[1]<-matching[1]+((any(currCell[[j]]==pair[1,1])&&any(currCell[[j]]==pair[1,2])))
            matching[2]<-matching[2]+((any(currCell[[j]]==pair[2,1])&&any(currCell[[j]]==pair[2,2])))
          }
          else if((sum((currCell[[j]])<0)==1)){
            matching[3]<-matching[3]+(sum(sort(currCell[[j]][(currCell[[j]])>=0])==pair[1,])==2)
            matching[4]<-matching[4]+(sum(sort(currCell[[j]][(currCell[[j]])>=0])==pair[2,])==2)
          }
        }
        if(sum(matching>0)==4){
          matchingIndex<-i
          break
        }
      }
      s<-rep(1,length(cells[[index]]))
      s2<-NULL
      cells<-cells[-matchingIndex]
    }
    else
      s<-NULL
    
    
    for(j in 1:length(currCell)){
      if(sum(currCell[[j]]<0)==0){
        
        cells[[index]]<-append(cells[[index]],list(currCell[[j]]))
        
        s<-c(s,-1)
        
      }
    }
    
    #    print(cells[[index]])
    #   print(s)
    cells1<-reOrderCell(cells[[index]],s)
    s<-cells1$sc
    cells[[index]]<-cells1$c1
    
    
    
    
    return(list("cells"=cells,"rmvd"=rmvd,"sign"=s))
  }
  
  trimList1<-function(cells){
    rmvd<-0
    toRM<-NULL
    toRM2<-NULL
    n<-length(cells)
    
    for(i in 1:n){
      vertices<-cells[[i]]
      #not infinite
      if(!isRelevant(vertices)){
        
        #      cells<-cells[-i]
        toRM<-c(toRM,i)
      }
      
    }
    if(!is.null(toRM))
      cells<-cells[-toRM]
    
    n<-length(cells)
    toRM<-NULL
    for(i in 1:(n-1)){
      cell1<-cells[[i]]
      
      for(j in (i+1):n){
        cell2<-cells[[j]]
        #not infinite
        if(cellsEqual(cell1,cell2)){
          # print(c(i,j))
          toRM<-c(toRM,j)
        }
      }
    }
    if(!is.null(toRM))
      cells<-cells[-toRM]
    
    
    return(cells)
  }
  trimList2<-function(cells){
    
    signs<-list()
    n<-length(cells)
    rmvd=NULL
    
    for(i in 1:n){
      #  print(i)
      # print(rmvd)
      index<-i-sum(rmvd<i)
      #   print("index")
      #  print(index)
      if(index<length(cells)){
        count<-0
        # print(index)
        vertices<-cells[[index]]
        toRM2<-NULL
        over<-FALSE
        for(j in 1:length(vertices)){
          
          if(!over)
            over<-(sum((unlist(vertices[[j]]))<0)==1)
          if(sum((unlist(vertices[[j]]))<0)>=2){
            toRM2<-c(toRM2,j)
          }
          else if(sum((unlist(vertices[[j]]))<0)==0)
            count<-count+1
        }
        if(!is.null(toRM2))
          cells[[index]]<-cells[[index]][-toRM2]
        if(over){
          tmp<-combineCells(index,cells,rmvd)
          cells<-tmp$cells
          rmvd<-tmp$rmvd
          signs<-append(signs,list(tmp$sign))
        }
        if(count==length(cells[[index]]))
          signs<-append(signs,list(rep(1,length(cells[[index]]))))
      }
      else if(index==length(cells)){
        signs<-append(signs,list(rep(1,length(cells[[index]]))))
        break
      }
      
    }
    
    
    
    return(list("cells"=cells,"signs"=signs))
  }
  getMed<-function(data1,cells,even=FALSE,slot=1){
    # open3d()
    cells<-reduceCells(cells)
    cells<-trimList2(cells)
    signs<-cells$signs
    cells<-cells$cells
    #
    weights<-matrix(0,nrow=nrow(data1),ncol=1)
    for(i in 1:length(cells)){
      #turn coordinates into polygon on sphere
      
      vertices<-getPolygon2(cells[[i]],data1,signs[[i]])
      if(!is.null(vertices)&&nrow(vertices)>2){
        #triangulate this polygon
        triangles<-triangulateConvex2(vertices)
        area<-getArea(triangles)
        triangles<-area$tris
        area<-area$area
        #get median(s)
        index<-calcMedTri(triangles[[1]],data1,even)
        # get area of polygon
        #  for(j in 1:length(triangles)){
        #  fillSphTriangle2(triangles[[j]],colors()[index[1]*10])
        #  }
        weights[index[slot],1]=weights[index[slot],1]+area
      }
    }
    weights<-weights/(2*pi)
    
    print(sum(weights))
    PM<-t(data1)%*%weights
    return(PM)
  }
  #i=9,11
  
  calcMed3dEff<-function(data1){
    planes<-point2plane(data1/max(data1))
    cells<-getMedLevel(planes,FALSE)
    #cells<-trimList1(cells)
    return(getMed(data1,cells))
    #combine ones that are "infinite"
    
  }
  calcMed3dEffEven<-function(data1){
    
    n<-nrow(data1)
    data1<-data1/max(data1)
    data12<-rbind(data1,c(0,0,-100))
    cells1a<-getMedLevel(point2plane(data12),T)
    #  cells1a<-trimList1(cells1a)
    PM1<-getMed(data12,cells1a,F)
    data12<-rbind(data1[1:n,],c(0,0,100))
    cells1a<-getMedLevel(point2plane(data12),T)
    #cells1a<-trimList1(cells1a)
    PM2<-getMed(data12,cells1a,F)
    
    return((PM1+PM2)/2)
  }
  if(nrow(data1)%%2==1)
    return(calcMed3dEff(data1))
  else
    return(calcMed3dEffEven(data1))
}
