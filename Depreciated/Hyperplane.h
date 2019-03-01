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
