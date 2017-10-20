/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   Level.h
 * Author: 12RAM
 *
 * Created on May 24, 2017, 9:30 AM
 */

#ifndef LEVEL_H
#define LEVEL_H
using namespace std;

class Level{
    
    int value;
    vector<double> point;
    
public:
    Level(){}
    Level(int lev,vector<double> loc){
    
        value=lev;
        point=loc;
    
    }
    
    int getValue(){
    
        return value;
    }
    
    vector<double> getPoint(){
    
        return point;
    }
	
	void setPoint(vector<double> p){
		point=p;
	}
	void print(){
		cout<<"level "<<value<<"\n";
		cout<<"point "<<point[0]<<" "<<point[1]<<" "<<point[2]<<"\n";
	}
    

};
#endif /* LEVEL_H */

