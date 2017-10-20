#include <vector>
#include "Hyperplane.h"
#include "Simplex.h"
#ifndef __Arr_INCLUDED__   // if x.h hasn't been included yet...
#define __Arr_INCLUDED__   //   #define this so the compiler knows it has been included


using namespace std;

class Arrangement {
    vector<Hyperplane> planes;
    //for each plane, for each cell, the simplices it intersects
    vector<vector<vector<bool> > > conflictLists;

public:

    Arrangement(vector<Hyperplane> plane) {
        planes = plane;
    };

    Arrangement() {
    };

    Arrangement(Hyperplane plane) {
        planes.push_back(plane);
    };

    vector<vector<vector<bool> > > getCLS() {

        return conflictLists;

    }

    //remove a simplex from lists

    void eraseCL(unsigned int ind, unsigned int indAbv) {

        vector<bool> tmp;
        unsigned int len;
        for (unsigned int i = 0; i < conflictLists.size(); ++i) {
    
      //      cout<<"list value: "<<conflictLists[i][indAbv][ind]<<" i="<<i<<" ";
            tmp=conflictLists.at(i).at(indAbv);
       //     cout<<"";
       //     printVec(tmp);
            len=tmp.size();
            tmp.erase(tmp.begin() + ind);
         //   printVec(tmp);
            
            conflictLists.at(i)[indAbv]=tmp;
         //   printCL();

        }


    }

    void printVec(vector<bool> tmp){
       for (unsigned int j = 0; j < tmp.size(); ++j) {
                cout<<"y "<<tmp[j]<<" ";
            }
    }
    
    unsigned int CLsize() {

        return conflictLists[0].size();
    }

    //add a simplex to lists

    void extendPCL(unsigned int len,unsigned int index,unsigned int cellIndex){
        for(unsigned int i=0;i<len;++i){
            conflictLists[index][cellIndex].push_back(false);
        }
    }
    void extendCL(unsigned int trianLength){
		
		vector<bool> tmp;
		
		for (unsigned int j = 0; j < trianLength; ++j){
                        tmp.push_back(false);
        }
		for (unsigned int i = 0; i < planes.size(); ++i){
			conflictLists.at(i).push_back(tmp);
		}
		
	}
	
    void addToCL(bool val, Hyperplane p, unsigned int ind, unsigned int indAbv) {

    //    printCL();
        for (unsigned int i = 0; i < planes.size(); ++i) {
            if (planes.at(i).equals(p)) {
                if (ind < conflictLists[i][indAbv].size())
                    conflictLists[i][indAbv][ind] = val;
                else {
                    for (unsigned int j = conflictLists[i][indAbv].size(); j < ind; ++j) {
                        conflictLists.at(i).at(indAbv).push_back(false);
                    }
                    conflictLists.at(i).at(indAbv).push_back(val);
                }

                break;
            }
        }
    }

    void push2List(vector<vector<bool> > addition) {
        conflictLists.push_back(addition);
    }

    void add(Hyperplane plane) {
        planes.push_back(plane);
    };

    Arrangement copy() {
        return Arrangement(planes);
    };

    void removeFP() {
        planes.erase(planes.begin());
    };
    
    void removeCellFromCL(unsigned int index) {
        for(unsigned int i=0;i<conflictLists.size();++i)
            conflictLists[i].erase(conflictLists[i].begin()+index);
    };

    void removeFCL() {
        conflictLists.erase(conflictLists.begin());
    };

    vector<Hyperplane> getPlanes() {
        return planes;
    };

    //         *Hyperplane getPlaneAdress(unsigned int index){
    //         
    //             return &planes[index];
    //         
    //         }

    Hyperplane getPlane(unsigned int index) {

        return planes[index];

    }

    unsigned int size() {
        return planes.size();
    }

    //		bool affected(vector<bool>) items{
    //			bool val;
    //			for(unsigned int i=0;i<items.size();++i){
    //				val=items[i];
    //				if(val)
    //					break;
    //			}
    //			
    //			return val;
    //			
    //		}

    void setList(vector<vector<vector<bool> > > cl) {

        conflictLists = cl;
    };

    vector<vector<bool> > getFCL() {

     //   printCL();
        return conflictLists.at(0);
    };

    void initLevel(Simplex *s) {
        vector<double> point;

        point = (*s).getLevel().getPoint();

        (*s).setLevel(getLevel(point));


    };

    Level getLevel(vector<double> point,int planeI) {
        double z;
        int level = 0;
		bool inf=planeI<0;
		
        for (unsigned int i = 0; i < planes.size(); ++i) {

		if(inf||i!=(unsigned int)planeI){
            z = planes.at(i).getZ(point.at(0), point.at(1));
            if (z < point.at(2))
                ++level;
        }
		else
			++level;
		}

        return Level(level,point);
    }
    
    Level getLevel(vector<double> point) {
      double z;
      int level = 0;
      
      for (unsigned int i = 0; i < planes.size(); ++i) {

          z = planes.at(i).getZ(point.at(0), point.at(1));
          if (z < point.at(2))
            ++level;

      }
      
      return Level(level,point);
    }

    void print() {
        for (std::vector<int>::size_type i = 0; i != planes.size(); i++) {
            planes[i].printP();
            cout << "\n";
        }
    }
    
    void printSize(){
        cout<<"size: "<<planes.size()<<"\n";
    }

    void printCLcell(unsigned int cellIndex){
        for (std::vector<int>::size_type i = 0; i != conflictLists.size(); ++i) {
                cout << conflictLists[i][cellIndex].size() << " is the number of simplices for plane"<<i<<"\n";
                for (std::vector<int>::size_type k = 0; k != conflictLists[i][cellIndex].size(); ++k) {
                    
                    cout << conflictLists[i][cellIndex][k] << " ";

                }
                cout << "\n";
        }
            
    }
    void printCLP(unsigned int index){
        for (std::vector<int>::size_type j = 0; j != conflictLists[index].size(); ++j) {
                cout << conflictLists[index][j].size() << " is the number of simplices\n";
                for (std::vector<int>::size_type k = 0; k != conflictLists[index][j].size(); ++k) {
                    
                    cout << conflictLists[index][j][k] << " ";

                }
                cout << "\n";
            }
    }
    
    void printCL() {


        for (std::vector<int>::size_type i = 0; i != conflictLists.size(); ++i) {
            cout << conflictLists[i].size() << " is the number of cells\n";
            printCLP(i);
            cout << "next Cell\n";
        }
    }
};

#endif 
