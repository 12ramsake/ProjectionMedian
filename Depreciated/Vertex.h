#include <vector>
#include <string>
#include <iostream>
#ifndef __Vertex_INCLUDED__   // if x.h hasn't been included yet...
#define __Vertex_INCLUDED__   //   #define this so the compiler knows it has been included
using namespace std;

class Vertex {
    vector<double> coord;
    vector<int> planesI;
    //it is always left or right inf
    bool unbounded;

public:

    Vertex() {
    }

    Vertex(vector<double> coor, bool unbounde, vector<int> tmp) {
        coord = coor;
        unbounded = unbounde;
        planesI = tmp;
    }

    Vertex(double X, double Y, double Z, bool unbounde, vector<int> tmp) {
        coord.push_back(X);
        coord.push_back(Y);
        coord.push_back(Z);
        planesI = tmp;
        unbounded = unbounde;
    }

    Vertex(vector<double> coor, bool unbounde, int tmp) {
        coord = coor;
        unbounded = unbounde;
        planesI.push_back(tmp);
    }

    Vertex(double X, double Y, double Z, bool unbounde, int tmp) {
        coord.push_back(X);
        coord.push_back(Y);
        coord.push_back(Z);
        planesI.push_back(tmp);
        unbounded = unbounde;
    }

    vector<double> getCoord() {

        return coord;

    }
    //finds common planes, usually for a new vertex

    vector<int> getCommon(Vertex v) {
        vector<int> tmp;
        for (unsigned int i = 0; i < planesI.size(); ++i) {
            for (unsigned int j = 0; j < v.planesI.size(); ++j) {
                if (planesI.at(i) == v.planesI.at(j)) {
                    tmp.push_back(planesI.at(i));
                    break;
                }
            }

        }
        return tmp;
    }

    double myRound(double num) {
        num = num * 1000;
        int myInt = num;
        double num2 = myInt / 1000;
        return num2;

    }

    bool equals(Vertex o) {

        bool lab;
        vector<double> oCoords=o.getCoord();
        bool val = oCoords.size() == coord.size();
        if (val) {
            for (unsigned int i = 0; i < coord.size() && val; ++i) {
                //=myRound(o.coord[i])==myRound(coord[i]);
                val = myRound(oCoords[i]) == myRound(coord[i]);
            }
        }
        
        lab=planesI.size()==o.planesI.size();
        for(unsigned int i = 0; i <planesI.size()&&lab; ++i){
            for(unsigned int j = 0; j <o.planesI.size(); ++j){
                lab=planesI[i]==o.planesI[j];
                if(lab)
                    break;
            }
        }

        return val&&lab;

    };

    bool isUnbounded() {
        for(unsigned int i=0;i<planesI.size();i++){
            unbounded=planesI[i]<0;
            if(unbounded)
                break;
        }
        
        return unbounded;

    }

    vector<int> getPlanesI(){
        return planesI;
    }
    
    
    void print(bool lastComma) {
        cout << "c(";

        for (unsigned int i = 0; i < coord.size(); ++i) {
            if (i < (coord.size() - 1))
                cout << coord[i] << ",";
            else
                cout << coord[i];
        }

        if (!lastComma)
            cout << "),\n";
        else
            cout << ")\n";
    }

};

#endif
