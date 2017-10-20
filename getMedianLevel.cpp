#include <vector>
#include "Canon.h"
#include <iostream>
#include <string>
//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
//#include <cmath>
using namespace Rcpp;
//cd ~/onedrive\ mac/OneDrive/Documents/research/Projection\ Median/3D/3d\ Eff/
//g++ -std=c++11 /Users/ramsayk3/onedrive\ mac/OneDrive/Documents/research/Projection\ Median/3D/3d\ Eff/debug.cpp -o a
//g++ -std=c++11 debug.cpp -o a
//./a
//using namespace std;
// [[Rcpp::export]]
std::vector<vector<vector<int > > > getMedLevel(arma::mat planes2,bool even2) {
  
  std::vector<vector<vector<int > > > faceList;

  
  //create planes
  std::vector<double> t1;
  std::vector<vector<double> > planes;
  
  for(unsigned int i = 0; i < planes2.n_rows; ++i){
    for(unsigned int j = 0; j < planes2.n_cols; ++j){
      t1.push_back(planes2(i,j));
    }
    planes.push_back(t1);
    t1.clear();
  }
  
  vector < vector<bool> > currCL;
  //for level
  Arrangement full = Arrangement(Hyperplane(planes.at(0), 0));
  //planes that are left
  Arrangement notIns;
  // The triangulation
  Canon tri;
  Hyperplane nextP;
  int kLevel;
  // set up test arrangement
  for (unsigned int i = 1; i < planes.size(); ++i) {
    full.add(Hyperplane(planes.at(i), i));
  }

  //if odd
  if(full.size() % 2==1)
    kLevel=(full.size()+1)/2;
  else if(!even2)
    kLevel=(full.size())/2;
  else
    kLevel=full.size()/2+1;
  //
  //   the list of planes not inserted
  notIns = full;
  tri = Canon(&notIns, kLevel);

  while(notIns.size()>0){
    
    // //  next plane to insert

    nextP = notIns.getPlanes().at(0);
    currCL = notIns.getFCL();
    notIns.removeFCL();
    // //   remove current plane from H\A(R)
    notIns.removeFP();
    // //    update triangulation	(ALL of step 1)
    // //   next plane to add, it cl, the H/A(R), A(H), level, A(R)

    tri.retri(nextP,currCL,&notIns,full,kLevel);

    // //     step two
    tri.removeInactive(&notIns);
    
  };
  //  cout <<"here"; 
  //faceList=tri.getFaceList();
  faceList=tri.finalTrimEff(kLevel,full);
  return faceList;
};


//Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
# 
# point2plane<-function(points){
#   planes<-matrix(0,nrow=nrow(points),ncol=4)
#   planes[,1]<-points[,1]*2
#   planes[,2]<-points[,2]*2
#   planes[,3]<--1
#   planes[,4]<-(-1)*points[,3]
#   return(planes)
# }
# set.seed(2543)
# #set.seed(2542473)
# matx<-matrix(replicate(3,rnorm(5)),nrow=5)
# # matx<-rbind(c(3,4.1,1),
# #             c(.2,6,1),
# #             c(2,1,0.2),
# #             c(1,3,-4.1),
# #             c(-1,3,5))+matrix(replicate(3,rnorm(5)),nrow=5)
# matx2<-point2plane(matx)
# #matx<-rbind(c(-1, 4, 1, 3),c(1, -1, 22, 1),c(2, 1, -2, -18))
# print(getMedLevel(matx2,FALSE))
# 
# #polys<-calcMed3dEff(matx)
# cells<-getMedLevel(matx2,FALSE)
# #c1<-trimList1(cells)
# 
# a<-proc.time()
# evenEx<-matrix(replicate(3,rnorm(81)),nrow=81)
# evenEx2<-point2plane(evenEx)
# b<-getMedLevel(evenEx2,FALSE)
# proc.time()-a
# #100 points in 252 seconds
# #print(getMedLevel(evenEx2,TRUE))
# for(i in 1:nrow(evenEx2)){
#   print(paste0("vector<double> t",i,"={",paste0(evenEx2[i,],collapse=","),"};"))
# }
# # 
# for(i in 1:nrow(evenEx2)){
#   print(paste0("t",i,","))
# }
# unname(mapply(paste0,rep(",t",20),1:20))
# a<-getMedLevel(evenEx2[1:6,],FALSE)
# # 

*/
