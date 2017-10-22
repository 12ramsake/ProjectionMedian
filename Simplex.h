
#include <vector>
#include "Vertex.h"
#include "Level.h"
#include "Hyperplane.h"
#include <cmath>
#ifndef __Simplex_INCLUDED__   // if x.h hasn't been included yet...
#define __Simplex_INCLUDED__   //   #define this so the compiler knows it has been included

using namespace std;

class Simplex {
    vector<Vertex> verts;
    Level level;
    vector<Hyperplane> conflictList;
public:

    Simplex() {
    }

    
    
        Simplex(Vertex v1, Vertex v2, Vertex v3, int lev) {

        verts.push_back(v1);
        verts.push_back(v2);
        verts.push_back(v3);
        level = Level(lev,mean());
    };

    Simplex(Vertex v1, Vertex v2, Vertex v3, Vertex v4, int lev) {

        verts.push_back(v1);
        verts.push_back(v2);
        verts.push_back(v3);
        verts.push_back(v4);
         level = Level(lev,mean());
    };

    Simplex(vector<Vertex > ver, int lev) {
        verts = ver;
        level = Level(lev,mean());
    };

    void addToCL(Hyperplane h) {

        conflictList.push_back(h);

    }
    void clearCL() {

        conflictList.clear();

    }

    vector<Hyperplane> getCL() {
        return conflictList;
    }

    Level getLevel() {
        return level;
    };

    unsigned int getDimension() {
        return verts.size() - 1;
    };

    void setLevel(Level l) {
        level = l;
    };

    vector<double> mean() {
        vector<double> avg;
        avg.assign(3, 0);
        for (unsigned int i = 0; i < verts.size(); ++i) {
            avg[0] = avg[0] + verts.at(i).getCoord()[0] / verts.size();
            avg[1] = avg[1] + verts.at(i).getCoord()[1] / verts.size();
            avg[2] = avg[2] + verts.at(i).getCoord()[2] / verts.size();
        }
        return avg;
    }

    void setCL(vector<Hyperplane> planes) {
        conflictList = planes;
    };

    int getSideLevel(Hyperplane h){
		
		int pLev=level.getValue();
		vector<double> p=level.getPoint();
		double z;
		z=h.getZ(p[0],p[1]);
		if(p[2]<z)
       pLev++;

      return pLev;
    }
    
    //gets the side of a hyperplane the vertex is on
    int getSide(unsigned int ind,Hyperplane h){
		
		vector<double> inSimplex=verts[ind].getCoord();
		double z;
		int side;
		z=h.getZ(inSimplex[0],inSimplex[1]);
		if(inSimplex[2]<z)
                    side=-1;
                else if(inSimplex[2]>z)
                    side=1;
                else
                    side=0;
                return side;
    }

    bool intersect(Hyperplane p) {
        bool inter;
        int side1;
        int side2;

        //for different pairs
        side1=getSide(0,p);
        for (unsigned int i = 1; i < verts.size(); ++i) {
            side2=getSide(i,p);
            inter=side2!=side1;
            if(inter)
                break;
        }
        return inter;

    }


    vector<double> crossProduct(vector<double> y1,vector<double> y2){
	
		vector<double> cp;
		cp={0,0,0};
		cp[0]=y1[1]*y2[2]-y1[2]*y2[1];
		cp[1]=y1[2]*y2[0]-y1[0]*y2[2];
		cp[2]=y1[0]*y2[1]-y1[1]*y2[0];

		return cp;		
	
	}
	
    Hyperplane getPlaneFromFacet(vector<int> inds){
		
		vector<double> y1,y2,x1,x2,x3,norm;
		double d;
	//	cout<<fixed;
		x1=verts[inds[0]].getCoord();
        x2=verts[inds[1]].getCoord();
        x3=verts[inds[2]].getCoord();
	//	print();
		for(unsigned int i=0;i<3;++i){
			y1.push_back(x1[i]-x3[i]);
			y2.push_back(x2[i]-x3[i]);
		}
	//	cout<<y1[0]<<" ,"<<y1[1]<<" ,"<<y1[2]<<"\n";
	//	cout<<y2[0]<<" ,"<<y2[1]<<" ,"<<y2[2]<<"\n";
		norm=crossProduct(y1,y2);
	//	cout<<norm[0]<<" ,"<<norm[1]<<" ,"<<norm[2]<<"\n\n";
		d=-norm[0]*x2[0]-norm[1]*x2[1]-norm[2]*x2[2];
		norm.push_back(d);
		
		return Hyperplane(norm,-8);
	}
	
	
	bool sameSide(vector<double > point,unsigned int indexNotInFacet,Hyperplane h){
		
		vector<double> inSimplex=verts[indexNotInFacet].getCoord();
		double z1,z2;
		bool below,above;
		z2=h.getZ(inSimplex[0],inSimplex[1]);
		z1=h.getZ(point[0],point[1]);
		below=point[2]<=z1&&inSimplex[2]<z2;
		above=point[2]>=z1&&inSimplex[2]>z2;
	//	cout<<z1<<" "<<point[2]<<" "<<z2<<" "<<inSimplex[2]<<"\n";
		return below||above;
	}
	
    bool isIn(vector<double > point){
        bool in;
		Hyperplane h;
		vector<int> c1,c2,c3,c4;
		vector<vector<int>> combos;

		c1={0,1,2};
		c2={0,1,3};
		c3={0,2,3};
		c4={1,2,3};
		combos={c4,c3,c2,c1};

		for(unsigned int i=0;i<4;++i){
			h=getPlaneFromFacet(combos[i]);
  //                      h.printP();
			in=sameSide(point,i,h);
			if(!in)
				break;
		}

		return in;        
    }

    bool isActive(int kLevel) {

      int K = (int)conflictList.size();
      int lower = level.getValue() - K-1;
      int upper = level.getValue() + K+1;

      
      bool active = kLevel >= lower && kLevel <= upper;

    //if(!active)
     // printVals( lower,  upper, kLevel, active );


      return active;

    }

	 void addVertex(Vertex v) {
        verts.push_back(v);
		level.setPoint(mean());
    }

    vector<Vertex> getVerts() {
        return verts;
    }

    bool isAdjacent(Simplex s) {
        bool value = false;
        int count = 0;
        unsigned int d1 = getDimension();
        unsigned int d2 = s.getDimension();

        //if 3 dimensional
        if (d1 == 3 && d1 == d2) {
            //find 3 verts that match
            for (unsigned int i = 0; i <= d2; ++i) {
                for (unsigned int j = 0; j <= d2; ++j) {
                    if (verts[i].equals(s.verts[j]))
                        ++count;
                }
            }
            value = count >= 3;
        }

        return value;
    }
	
    void printVals(int lower, int upper,int kLevel,bool active ){
        print();
        cout<<"active "<<active<<" kLevel "<<kLevel<<" lower "<<lower<<" upper "<<upper<<"\n";
        }
 

   

    void printLevel(){
		cout<<" "<<level.getValue()<<" ";
	}
	void print(string begin) {
        cout << begin << "rbind(";
        for (unsigned int i = 0; i < verts.size(); ++i) {

            verts[i].print(i == verts.size() - 1);


        }
        cout << ")\n";
    }
	
	void print() {
        cout << "Simplex<-rbind(";
        for (unsigned int i = 0; i < verts.size(); ++i) {

            verts[i].print(i == verts.size() - 1);


        }
        cout << ")\n plotS() \n points3d("<<level.getPoint()[0]<<","<<level.getPoint()[1]<<","<<level.getPoint()[2]<<")\n";
    }

    void printCL() {
        for (unsigned int i = 0; i < conflictList.size(); ++i) {

            conflictList[i].printP();

        }
        cout << "\n";
    }


};
#endif
