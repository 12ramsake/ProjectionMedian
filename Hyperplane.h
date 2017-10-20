#include <string>
#include <iostream>
#include <vector>
#ifndef __Hyperplane_INCLUDED__   // if x.h hasn't been included yet...
#define __Hyperplane_INCLUDED__   //   #define this so the compiler knows it has been included
//

using namespace std;

class Hyperplane{
      
  vector <double> coef;
   int position;
 public:
   Hyperplane (double a,double b,double c,double d){
     coef.push_back(a);
     coef.push_back(b);
     coef.push_back(c);
     coef.push_back(d);
   };
   Hyperplane(){
   }
   Hyperplane (vector <double> coeff,int pos){
     position=pos;
     coef=coeff;
   };
   double getD(){
     return coef.at(3);
   };
   vector<double> getCoef(){
   
   return coef;
   
   }; 
   
    int pos(){
        return position;
    }
   //returns a point on the intersection line between 2 planes, given the y coor
/*	vector<double> interPointY(double Y,Hyperplane p){
		vector<double> coord;
		double zratio=p.coef[2]/coef[2];
		vector<double> tmp={zratio*coef[0],zratio*coef[1],zratio*coef[3]};
		double X;
	
	//    cout<<p.coef[2]<<"  "<<coef[2];
	//    cout<<" "<<zratio<<"\n";
	    
	    
		X=Y*(tmp[1]-p.coef[1])+(tmp[2]-p.coef[3]);
		X=X/-(tmp[0]-p.coef[0]);
		coord.push_back(X);
		coord.push_back(Y);
		coord.push_back(getZ(X,Y));
		
		// cout<<X<<" "<<Y<<" "<<coord[2]<<"\n";
		
		return coord;
	}
	
	vector<double> interPointX(double X,Hyperplane p){
		vector<double> coord;
		double zratio=p.coef[2]/coef[2];
		vector<double> tmp={zratio*coef[0],zratio*coef[1],zratio*coef[3]};
		double Y;
	
	//    cout<<p.coef[2]<<"  "<<coef[2];
	//    cout<<" "<<zratio<<"\n";
	    
	    
		Y=(X*-(tmp[0]-p.coef[0])-(tmp[2]-p.coef[3]))/(tmp[1]-p.coef[1]);

		coord.push_back(X);
		coord.push_back(Y);
		coord.push_back(getZ(X,Y));
		
		// cout<<X<<" "<<Y<<" "<<coord[2]<<"\n";
		
		return coord;
	}
	
	vector<double> interPointZ(double Z,Hyperplane p){
		vector<double> coord;
		double xratio=p.coef[0]/coef[0];
		vector<double> tmp={xratio*coef[1],xratio*coef[2],xratio*coef[3]};
		double Y;
	
	    cout<<p.coef[2]<<"  "<<coef[2];
	    cout<<" "<<xratio<<"\n";
	    
	    
		Y=(Z*-(tmp[1]-p.coef[2])-(tmp[2]-p.coef[3]))/(tmp[0]-p.coef[1]);


		coord.push_back(getX(Y,Z));
		coord.push_back(Y);
		coord.push_back(Z);		
		// cout<<X<<" "<<Y<<" "<<coord[2]<<"\n";
		
		return coord;
	}
   */
   //returns d- dot product of normal and point
   double dmDotProd(vector<double> p){
   	double prod;
   	
   		for(unsigned int i=0;i<3;++i){
   			prod=p[i]*coef[i];
   			
		   }
		   return -coef[3]-prod;
   	
   }
   
   double dotProd(vector<double> p){
   	double prod=0;
   	
   		for(unsigned int i=0;i<3;++i){
   			prod=prod+p[i]*coef[i];
		   }
		   return prod;
   	
   }
   
   double getRayT(vector<double> r0,vector<double> rd){
	   double dp0,dpr;
	   dp0=dotProd(r0);
	   dpr=dotProd(rd);
		if(dpr==0){
			rd[2]=rd[2]+0.00000001;
			dpr=dotProd(rd);
		}
			

		return -(dp0+coef[3])/dpr;

	   
   }
   
   
   bool equals(Hyperplane p){
	   return coef==p.coef;
   }
   
   bool equals2(Hyperplane p){
       return p.position==position;
   }
   
   bool in(vector<Hyperplane> A){
   	bool val=false;
   	for(unsigned int i=0;i<A.size();++i){
   		val=coef==A.at(i).coef;
   		if(val)
                    break;
	   }
   	return val;
   }
   
  
   
   double getZ(double x, double y){
   		return (x*coef.at(0)+y*coef.at(1)+coef.at(3))/-coef.at(2);
   };
    double getX(double y, double z){
   		return (z*coef.at(2)+y*coef.at(1)+coef.at(3))/-coef.at(0);
   };
   double getY(double x, double z){
      		return (z*coef.at(2)+x*coef.at(0)+coef.at(3))/-coef.at(1);
   };
   
   void printP(){
        cout<<coef.at(0)<<", "<<" "<<coef.at(1)<<", "<<coef.at(2)<<", "<<coef.at(3)<<"\n";
     }
};
//int main(void){return 0;};


#endif 
