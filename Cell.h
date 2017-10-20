#include <vector>
#include "Polygon.h"
#include "Simplex.h"
#include "Arrangement.h"
#include "AdjGraph.h"
#include <numeric>
#include <algorithm>
#include <iostream>
#include <string>
#include <cmath>

using namespace std;

class Cell {
    vector<Polygon> faces;
    AdjGraph adj;
    vector<Simplex> trian;
    Vertex bv;
    unsigned int active;


public:

    Cell() {
    }

    Cell(vector<Polygon> face, bool checkActives = false) {

        //bv and adjacencies set in triangulate cell
        faces = face;
        //      printFaces();
        triangulateCell();
        if (checkActives)
            active = trian.size();
        else
            active = 0;


    }

    void getBV() {

        Vertex curr;
        bv = faces.at(0).getbvV();

        for (unsigned int i = 1; i < faces.size(); ++i) {

            curr = faces.at(i).getbvV();
            if (curr.getCoord()[0] < bv.getCoord()[0])
                bv = curr;
            else if (curr.getCoord()[0] == bv.getCoord()[0]) {

                if (curr.getCoord()[1] < bv.getCoord()[1])
                    bv = curr;

                else if (curr.getCoord()[1] == bv.getCoord()[1]) {

                    if (curr.getCoord()[2] < bv.getCoord()[2])
                        bv = curr;
                }

            }
        }
   
    }

    vector<Polygon> getFaces() {
        return faces;
    }
    
    vector<Simplex> getTrian() {
        return trian;
    }

    void triangulateCell() {


        //assume first face has bv, verified...
        //TRIANGULATE ALL NON INCIDENT FACES first

        vector<Simplex> currS;
        Polygon curF;

        unsigned int sum;
        //unsigned int tmp;

        bool notEqualVertex;



        getBV();

        //for each face
        for (unsigned int i = 0; i < faces.size(); ++i) {

            curF = faces[i];
            //if the face does not contain the bottom vertex face, triangulate it in 2D

            notEqualVertex = !bv.equals(curF.getbvV());
            if (notEqualVertex) {
                currS = curF.triangulate();


                //for each simplex in the 2D triangulation
                for (unsigned int j = 0; j < currS.size(); ++j) {
                    //add bottom vertex
                    currS.at(j).addVertex(bv);
                    trian.push_back(currS.at(j));
                }
            }
        }
        sum = trian.size();
        //you want to create an empty graph
        adj = AdjGraph(trian.size());
        //    adj.print();


        for (unsigned int i = 0; i < trian.size() - 1; ++i) {
            adj.set(1, i, i);
            for (unsigned int j = (i + 1); j < trian.size(); ++j) {
                sum = adj.getRowSum(i);
                //			
                //if all adjacencies found
                if (sum == 4)
                    break;
                    //					//check for adjacency
                else if (trian[i].isAdjacent(trian[j])) {
                    adj.set(1, i, j);
                }
            }
        }
        adj.set(1, trian.size() - 1, trian.size() - 1);
        //    adj.print();
        //end
    }

    //initializes a cells conflict list....

    vector<bool> initCL(Hyperplane p) {


        vector<bool> newCL;

        for (unsigned int i = 0; i < trian.size(); ++i) {
            if (trian.at(i).intersect(p)) {
                newCL.push_back(true);
                trian.at(i).addToCL(p);
            } else
                newCL.push_back(false);
        }

        return newCL;

    };

    Simplex getFirstSimplex() {
        return trian.at(0);
    };

    bool is2D() {
        unsigned int t = 3;
        bool is2d;
        for (unsigned int i = 0; i < trian.size(); ++i) {
            is2d = trian.at(i).getDimension() < t;
            if (!is2d)
                break;
        }
        return is2d;
    };

    //adds in right order to make a face
    //here AUGUST 21 THIS IS STILL WRONG PLEASE FIX

    void order(vector<Vertex> *toAdd) {

        
        vector<Vertex> copy = (*toAdd);
        vector<Vertex> front;
        vector<double> angles, a2;
        vector<double> avg;

        if (copy.size() > 2) {
            avg.assign(3, 0);
            for (unsigned int i = 0; i < copy.size(); ++i) {
                avg[0] = avg[0] + copy.at(i).getCoord()[0] / copy.size();
                avg[1] = avg[1] + copy.at(i).getCoord()[1] / copy.size();
                avg[2] = avg[2] + copy.at(i).getCoord()[2] / copy.size();
            }




            for (unsigned int i = 0; i < copy.size(); ++i) {
                angles.push_back(getAngle(Vertex(avg, false, -1), copy[i]));
            }

            a2 = angles;
            std::sort(a2.begin(), a2.end(), gThan);
            for (unsigned int i = 0; i < a2.size(); ++i) {
                for (unsigned int j = 0; j < angles.size(); ++j) {
                    if (angles[j] == a2[i]) {
                        front.push_back(copy[j]);
                        break;
                    }
                }
            }
            (*toAdd) = front;
        }
    }
    //gets angle between A and B, with resp to init

    bool static gThan(double a, double b) {
        return a>b;
    }

    double getAngle(Vertex init, Vertex A) {
        double X, Y, angle;

        X = A.getCoord()[0] - init.getCoord()[0];
        Y = A.getCoord()[1] - init.getCoord()[1];

        //        for (unsigned int i = 0; i < 3; ++i) {
        //            tmpA = A.getCoord()[i] - init.getCoord()[i];
        //            dotProd = dotProd + tmpA*tmpB;
        //            dA = dA + pow(tmpA, 2);
        //            dB = dB + pow(tmpB, 2);
        //        }
        angle = atan2(Y, X);
        if (angle < 0)
            angle = angle + 2 * 3.141592653589793238462643383279502884;

        return angle;
    }


    // gets intersections on plane and simplex


    //is k in vector twice

    bool isInTwice(unsigned int k, vector<unsigned int> v) {
        int count = 0;
        for (unsigned int i = 0; i < v.size(); ++i) {

            if (v[i] == k)
                ++count;
            if (count == 2)
                break;
        }

        return count == 2;
    }

    void updateAdjMatrix(vector<unsigned int> cand, unsigned int newSimplexStart) {

        unsigned int sum;
        int extendBy = trian.size() - newSimplexStart;
        //      numOld=cand.size();
        if (extendBy > 0)
            adj.extend(extendBy);
        //        adj.printDims();
        //for each new simplex
        for (unsigned int i = newSimplexStart; i < trian.size() - 1; ++i) {
            adj.set(1, i, i);
            //find adjacency relations among other new simplices
            for (unsigned int j = (i + 1); j < trian.size(); ++j) {
                sum = adj.getRowSum(i);
                if (sum == 4)
                    break;
                else if (trian[i].isAdjacent(trian[j]))
                    adj.set(1, i, j);
            }

            //find adjacency relations among other old simplices
            for (unsigned int j = 0; j < cand.size(); ++j) {
                sum = adj.getRowSum(i);
                if (sum == 4)
                    break;

                else if (cand[j] < trian.size()) {

                    if (trian.at(i).isAdjacent(trian.at(cand[j]))) {
                        adj.set(1, i, cand[j]);
                    }
                }
            }
        }
        adj.set(1, trian.size() - 1, trian.size() - 1);
  
    }

    bool inNew(Polygon f, Hyperplane h) {

        bool val = !f.getbvV().equals(bv);

        double w;

        //if we think is in new face

        if (val) {
            //
            w = f.getWeight2(h, f.getbvV().getCoord(), bv.getCoord());
            val = w < 1 && w > 0;
        }

        return val;
    }

    bool isAdj(Cell c) {

        bool sameFace = false;
        for (unsigned int i = 0; i < faces.size(); ++i) {
            for (unsigned int j = 0; j < c.faces.size(); ++j) {
                sameFace = faces[i].equals(c.faces[j]);
                if (sameFace)
                    break;
            }
            if (sameFace)
                break;
        }
        return sameFace;
    }

    Cell split(Hyperplane p, unsigned int sim, vector<bool> CL, Arrangement *notIns, Arrangement full, unsigned int cellIndex, int kLevel) {


          //new cell
        Cell newC = Cell();
        //new simplices
        vector<Simplex> newS;
        //new simplices for new cell
        vector<Simplex> newC1;
        vector<Simplex> oldS;
        //candidates for possible adjacency changes
        vector <unsigned int> cand;
        vector<unsigned int> toRmv;
        //int face is the new face created by the plane
        Polygon intFace;
        //the old adjacency graph
        AdjGraph oldAdj = adj;
        //new faces, old faces, and new cell faces (C'' in paper)
        vector<Polygon> cDblP;
        vector<Polygon> oldFaces = faces;
        Polygon newFace;
        bool isIn;

        //??
        vector<unsigned int> inter;
        //simplices removed
        vector<unsigned int> rmvd;
        unsigned int rCounter = 0;


        //incrementor
        //int inc = 0;
        int n = 0;
        //index for loop for cand
        unsigned int newSimplexStart;
        //simplices not affected
        vector<unsigned int> notAff;
        //do 2 faces intersect
        //    bool intersects;
        vector<Vertex > tmpVerts;
        vector<Vertex > newFaceVerts;

     
        //remove old simplices, store them and delete from old conflict lists
        //We need the union of the conflict lists from the destroyed ones
        //


        for (unsigned int i = sim; i < CL.size(); ++i) {

            //if intersect

            if (CL[i]) {
                //decrement active if this simplex was active
                if (trian.at(i - rCounter).isActive(kLevel))
                    --active;

                //add this to removed
                oldS.push_back(trian.at(i - rCounter));

                //erase this simplex
                trian.erase(trian.begin() + i - rCounter);

                //remove this row and column
                adj.rmvNode(i - rCounter);


                //erase from conflict list
                (*notIns).eraseCL(i - rCounter, cellIndex);
                //add this index
                rmvd.push_back(i);
                rCounter = rmvd.size();
            }
            else {
                notAff.push_back(i);
            }
        }

        //cout<<"2\n";
        // 		add possible adjacencies to a list
        for (unsigned int i = 0; i < CL.size(); ++i) {
            for (unsigned int j = 0; j < rmvd.size(); ++j) {
                if (oldAdj.getRow(i)[rmvd[j]] == 1) {
                    //if not on diagnol
                    if (i != rmvd[j]){
                         rCounter = 0;
                        for (unsigned int k = 0; k < rmvd.size(); ++k) {
                         if(rmvd[k]<i)
                            rCounter++;   
                         else
                            break;
                        }
                        cand.push_back(i-rCounter);
                    }
                    break;
                }
            }
        }

        //   if(cellIndex==3){
        //     (*notIns).printCL();
        // }


        newSimplexStart = trian.size();
        //for each face, split if necessary
        for (unsigned int i = 0; i < faces.size(); ++i) {
            // n = tmpVerts.size();

            //        faces[i].print(",");
            //here is the problem: August 18
            //		cout<<"22\n";
            newFace = faces[i].getFace(p, oldS, &tmpVerts, bv);

            //            			cout<<"3\n";
            ////		            //if this face splits...
            if (tmpVerts.size() > 1) {
                //                cout << "\n" << tmpVerts.size() << "\n";
                for (unsigned int j = 0; j < tmpVerts.size(); ++j) {
                    isIn = in(tmpVerts[j], newFaceVerts);
                    if (!isIn) {
                        newFaceVerts.push_back(tmpVerts.at(j));
                    }
                }
                //
                //     		            get new simplices for the old split face
                //			cout<<"4\n";
                newS = faces[i].getNewS(tmpVerts);

                //
                // 		            //connect to bottom vertex if not already
                //ADD TO TRIANGULATION
                if (!faces[i].getbvV().equals(bv)) {

                    for (unsigned int j = 0; j < newS.size(); ++j) {
                        newS.at(j).addVertex(bv);
                        trian.push_back(newS.at(j));
                        cand.push_back(trian.size() - 1);
                        //  newS[j].print("a");
                    }
                }
                //
                //
                // 		            add faces to new cells vector

                cDblP.push_back(newFace);
                tmpVerts.clear();

            }//
                // 		            if the face is not intersected
            else if (inNew(faces[i], p)) {
                //			cout<<"5\n";
                cDblP.push_back(faces[i]);
                toRmv.push_back(i);
            }
        }
        //delete faces that went to the new cell
        //		cout<<"6\n";
        for (unsigned int j = 0; j < toRmv.size(); ++j) {

            faces.erase(faces.begin() + toRmv[j] - n);
            n = n + 1;
        }
        //order vertices in new face made by inserted plane
        //		cout<<"7\n";
        //   if(newFaceVerts.size()==0){
        //     p.printP();
        //   cout<<"ind "<<cellIndex<<"\n";
        // }        

        order(&newFaceVerts);
        //
        //		        //make new face wiith ordered verts
        intFace = Polygon(newFaceVerts, false);
        //both cells have this face
        //	cout<<"8\n";
        faces.push_back(intFace);
        cDblP.push_back(intFace);
        //// 		        //now triangulate new face and add those simplices
        //new c1 is the simplices of the new face
        newC1 = intFace.triangulate();
        //add them to the current triangulation
        //		cout<<"9\n";
        for (unsigned int j = 0; j < newC1.size(); ++j) {
            newC1.at(j).addVertex(bv);
            trian.push_back(newC1.at(j));
            //candidates for the adjacency matrix
            cand.push_back(trian.size() - 1);
            //
        }
        // 
        //    cout<<"10\n";		
        updateAdjMatrix(cand, newSimplexStart);
        //
        //	
        //    cout<<"11\n";	    
        newC = Cell(cDblP);
        //
        //		    //update conflict lists...


       updateCL(oldS, &newC, notIns, cellIndex, newSimplexStart, oldFaces, p);

    //    bool toggle=(*notIns).size()==0&&cellIndex==307;

       // if((*notIns).size()==0&&cellIndex==39)
        //    cout<<cellIndex<<"\n";
        updateLevels(newSimplexStart, oldS[0], kLevel, &newC,p);
        //		cout<<"15";
        return newC;
    };
    //here

    void updateLevels(unsigned int newSimplexStart, Simplex oldS, int kLevel, Cell *newCell, Hyperplane p) {

        Level oldLev = oldS.getLevel();

        bool start;
        int index = -1;
        
   //     if(toggle){
    //        printTrian();
    //        (*newCell).printTrian();
    //        oldLev.print();
     //   }
        
        //get which simplex contains this point
        for (unsigned int i = newSimplexStart; i < trian.size(); ++i) {
            start = trian[i].isIn(oldLev.getPoint());

            if (start) {
                index = i;
                trian.at(index).setLevel(oldLev);
                if(trian.at(index).isActive(kLevel))
                        active++;
                break;
            }
        }
        //trian[index].setLevel(oldLev);

        if (start) {

            traverseForLevels3(index, newSimplexStart,  kLevel);
          //  printLevels();
            index=(*newCell).startIndex(getTrian(),kLevel,p,newSimplexStart,0);
            (*newCell).traverseForLevels3(index, 0, kLevel);
           // (*newCell).setLevelsForCell(p, oldLev.getValue(), oldLev.getPoint(), 0, kLevel,planeList,full);
        } else {
        //    cout << "\n BBBBBBBBBBBBBBBBBBBBB \n";
            for (unsigned int i = 0; i < (*newCell).trian.size(); ++i) {
                start = (*newCell).trian[i].isIn(oldLev.getPoint());
              
                if (start) {
                      index = i;
                    (*newCell).trian.at(index).setLevel(oldLev);
                    if((*newCell).trian.at(index).isActive(kLevel))
                       (*newCell).active++;
                    break;
                }
            }

            (*newCell).traverseForLevels3(index, 0, kLevel);
          //  (*newCell).printLevels();
            index=startIndex((*newCell).getTrian(),kLevel,p,0,newSimplexStart);
            traverseForLevels3(index, newSimplexStart, kLevel);
           // setLevelsForCell(p, oldLev.getValue(), oldLev.getPoint(), newSimplexStart, kLevel,planeList,full);
        }
    }

//     void setLevelsForCell(Hyperplane p, int initLevel, vector<double> pInit,  unsigned int start, int kLevel,vector<Hyperplane> planeList,Arrangement full) {
// 
//         Level curr;
//         planeList.push_back(p);
//         for (unsigned int i = start; i < trian.size(); ++i) {
// 
//             curr = getNewLev(initLevel, planeList, trian.at(i).getLevel().getPoint(), pInit,full);
//             trian.at(i).setLevel(curr);
//             if (trian.at(i).isActive(kLevel)) {
//                 active++;
//             }
//         }
//     }
//     
    int startIndex(vector<Simplex> triann,int kLevel,Hyperplane p,unsigned int nSS1,unsigned int nSS2){
        Level curr;
        vector<Hyperplane> planeList;
        int index=-1;
        bool tmp;
        for(unsigned int i=nSS1;i<triann.size();i++){
         for(unsigned int j=nSS2;j<trian.size();j++){
            tmp=triann[i].isAdjacent(trian[j]);
            if(tmp){
            //adj.dummy();
                index=j;
                planeList=triann[i].getCL(); 
                planeList.push_back(p);
                    for(unsigned int k=0;k<trian[j].getCL().size();k++){
                        if(!inHyp(trian[j].getCL()[k],planeList))
                            planeList.push_back(trian[j].getCL()[k]);                            
                    }
                  
                    curr = getNewLev(triann[i].getLevel().getValue(), planeList,trian.at(j).getLevel().getPoint(), triann[i].getLevel().getPoint());
                    trian.at(j).setLevel(curr);
                    if (trian.at(j).isActive(kLevel)) {
                        active++;
                    }
                 //   adj.dummy();
               break;
            }
         }
         if(tmp)
         break;
        }

        return index;
    }

//     void traverseForLevels2(unsigned int start, unsigned int newSimplexStart, Level oldLev, int kLevel,vector<Hyperplane> planeList,Arrangement full) {
// 
//         Level curr;
//         //visited vertices
//         vector<double> initP = trian[start].getLevel().getPoint();
//         int initLevel = trian[start].getLevel().getValue();
//        
// 
// 
//         for (unsigned int i = newSimplexStart; i < trian.size(); ++i) {
//             if (i != start) {
//                 
//                 curr = getNewLev(initLevel, planeList, trian.at(i).getLevel().getPoint(), initP,full);
//                 trian.at(i).setLevel(curr);
//                 if (trian.at(i).isActive(kLevel)) {
//                     active++;
//                 }
//             }
//         }
// 
//     }
//     
    bool inHyp(Hyperplane c, vector<Hyperplane > vv) {

        bool in = false;
        unsigned int count = 0;
        while (!in && count < vv.size()) {


            in = vv[count].equals(c);
            count = count + 1;
        }

        return in;
    }
   
    void traverseForLevels3(unsigned int start, unsigned int newSimplexStart,  int kLevel) {

        Level curr;
        //visited vertices
        vector<Hyperplane> planeList;
        unsigned int star=start;
        Simplex prev;
        vector<double> initP;
        int initLevel;
        vector<unsigned int> processed={start};
        vector<unsigned int> queue;
        unsigned int place;
        AdjGraph adjSub = adj.getSub(newSimplexStart);
//         adjSub.print();
//         cout<<"\n new simplex start "<<newSimplexStart;
        vector<unsigned int>  adjRelations;
       

        while (processed.size()<(trian.size()-newSimplexStart)) {
         
        //set old simplex and update values
//              cout<<"processed \n";
//             for(unsigned int i=0;i<processed.size();++i){
//                 cout<<i<<" ";
//             }
//             cout<<"\n";
            prev=trian[star];
          //  cout<<"prev "<<star<<"\n";
            adjRelations=adjSub.getInter(star-newSimplexStart);
            initP = prev.getLevel().getPoint();
            initLevel = prev.getLevel().getValue();
            //adj.dummy();
          //  cout<<adjRelations.size()<<"size\n";
            
            for(unsigned int i=0;i<adjRelations.size();++i){
                 place=adjRelations[i]+newSimplexStart;
               //  cout<<"place "<<place<<"\n"; 
        //if not processed
                if (!unIntIn(place,processed)) {
                
                    processed.push_back(place);
                    queue.push_back(place); 
        
                  //  adj.dummy(); 
                 //combine conflict lists
                    planeList=prev.getCL(); 
                    for(unsigned int j=0;j<trian[place].getCL().size();++j){
                        if(!inHyp(trian[place].getCL()[j],planeList))
                            planeList.push_back(trian[place].getCL()[j]);                            
                    }
                  
                    curr = getNewLev(initLevel, planeList, trian.at(place).getLevel().getPoint(), initP);
                    trian.at(place).setLevel(curr);
                    if (trian.at(place).isActive(kLevel)) {
                        active++;
                    }
                }
            }
            if(queue.size()>0){
                star=queue[0];
                queue.erase(queue.begin()+0);
             }
        }

    }

    Level getNewLev(int prevLev, vector<Hyperplane> planeList, vector<double> p, vector<double> pInit) {

        int newLev = prevLev;
//         int tmp;
   //     int planes=0;
        double z, zInit;
//    
//        tmp = (full).getLevel(pInit).getValue();
//        if(newLev!=tmp){
//         cout<<"old "<<newLev<<"true "<<tmp<<"\n";
//        }

        for (unsigned int i = 0; i < planeList.size(); ++i) {


            z = planeList[i].getZ(p[0], p[1]);
            zInit = planeList[i].getZ(pInit[0], pInit[1]);
            //plane is above old one but below new one
            if (z <= p[2] && zInit > pInit[2]) {
                newLev++;
            } 
            
            else if (z > p[2] && zInit <= pInit[2]) {
                newLev--;
            }
	
        }
//    
//        tmp = (full).getLevel(p).getValue();
//        if(newLev!=tmp){
//         cout<<"new "<<newLev<<"true "<<tmp<<"\n";
//        }
   
        return Level(newLev, p);

    }

    bool in(Vertex c, vector<Vertex > vv) {

        bool isIn = false;
        unsigned int count = 0;

        while (!isIn && count < vv.size()) {


            isIn = vv[count].equals(c);
            count = count + 1;
        }

      
        return isIn;
    }

    bool toRight(Cell c) {

        return bv.getCoord().at(0) > c.bv.getCoord().at(0);

    }

    //takes old conflict lists and new cell NOT DONE
    //takes the new cell, union of destroyed lists (old simplices), the not inserted arrangement and the cell index in triangulation
    //

    void updateCL(vector<Simplex> oldS, Cell *cdp, Arrangement *notIns, unsigned int cellIndex, unsigned int newSimplexStart, vector<Polygon> oldFaces, Hyperplane old) {

        vector<Hyperplane> currCL;
        vector<Hyperplane> complete, completeCompliment;
        vector < vector<bool> > newCL;
        // bool tmp;
        Hyperplane h;
        //vector<unsigned int> init;


          //For each destroyed simplex (get the list of hyperplanes)
        for (unsigned int i = 0; i < oldS.size(); ++i) {
            //get it's conflict list
             currCL = oldS.at(i).getCL();
            //for each plane in it
            for (unsigned int j = 0; j < currCL.size(); ++j) {
                h = currCL.at(j);
                       
                //if h is not in the new one add it
                if (!h.equals(old)) {
                    if (!h.in(complete)) {

                        complete.push_back(h);
                    }
                }
            }
            if (complete.size() == (*notIns).size())
                break;
        }

        if (complete.size() != (*notIns).size()) {
            for (unsigned int i = 0; i < (*notIns).size(); ++i) {
                if (!((*notIns).getPlane(i)).in(complete)) {
                    (*notIns).extendPCL(trian.size() - newSimplexStart, i, cellIndex);
                }
            }
        }
        //get initial simplex for each hyperplane,
       // init = getInit(newSimplexStart, complete, oldFaces);


        if ((*notIns).size() > 0) {
            
             upCLOld(complete, notIns, cellIndex, newSimplexStart);
            (*cdp).upCLNew(complete, notIns);

        }            //if empty
        else {
            for (unsigned int i = 0; i < trian.size(); ++i)
                trian.at(i).clearCL();
        }

     //   return complete;
    }

    //
    void upCLOld(vector<Hyperplane> H, Arrangement *notIns, unsigned int n, unsigned int newSimplexStart) {

        Hyperplane h;
        bool inter;
        Simplex S;

        for (unsigned int i = newSimplexStart; i < trian.size(); ++i) {
            for (unsigned int j = 0; j < H.size(); ++j) {
                S = trian.at(i);
                h = H.at(j);
                //do they intersect?
                inter = S.intersect(h);
                if (inter) {
                    trian.at(i).addToCL(h);
                }
                //add this to conflict list
                (*notIns).addToCL(inter, h, i, n);
            }
        }


    }

    //for new cell

    void upCLNew(vector<Hyperplane> H, Arrangement *notIns) {

        Hyperplane h;
        bool inter;
        Simplex S;
        unsigned int n = (*notIns).CLsize();
        (*notIns).extendCL(trian.size());

        for (unsigned int i = 0; i < trian.size(); ++i) {
            for (unsigned int j = 0; j < H.size(); ++j) {
                S = trian.at(i);
                h = H.at(j);
                //do they intersect?
                inter = S.intersect(h);
                if (inter) {
                    trian.at(i).addToCL(h);
                }
                //add this to conflict list
                (*notIns).addToCL(inter, h, i, n);
            }
        }


    }


    //get initial simplices fort each hyperplane in union of lists

    vector<unsigned int> getInit(unsigned int newSimplexStart, vector<Hyperplane> H, vector<Polygon> oldFaces) {
        vector<bool> val;
        vector<unsigned int> init;
        //initialize
        init.assign(H.size(), 0);
        val.assign(H.size(), false);
        //does it intersect in an edge?
        for (unsigned int i = newSimplexStart; i < trian.size(); ++i) {
            for (unsigned int j = 0; j < H.size(); ++j) {
                val[j] = intersectsEdge(trian[i], H[j], oldFaces);
                if (val[j]) {
                    init[j] = i;
                }

            }
        }

        for (unsigned int j = 0; j < H.size(); ++j) {
            if (!val[j]) {
                val[j] = true;
                init[j] = trian.size() - 1;
            }
        }

        return init;

    }
    //you need to get the  OLD FACES and check those edges....
    //after that use the intersecting one

    bool intersectsEdge(Simplex s, Hyperplane h, vector<Polygon> oldFaces) {
        vector<vector<Vertex> > tmp1;
        vector<Vertex> curr;
        bool edge = false;
        //does h intersect s?
        tmp1 = s.intersectionVerts(h);
        //if so is it an edge of C?
        if (tmp1.size() > 0) {
            for (unsigned int i = tmp1.size(); i < tmp1.size(); ++i) {
                curr = tmp1[i];
                for (unsigned int j = 0; j < oldFaces.size(); ++j) {
                    edge = oldFaces[j].isEdge(curr);
                    if (edge)
                        break;
                }
                if (edge)
                    break;
            }

        }

        return edge;
    }

    int getActive() {
        return active;
    }

    void initLevels(Arrangement r, int kLevel) {

        for (unsigned int i = 0; i < trian.size(); ++i) {

            r.initLevel(&trian.at(i));
            if ((trian.at(i)).isActive(kLevel))
                ++active;
        }
    }
    //we must traverse S....
    
    


//     void traverse(unsigned int init, Hyperplane h, Arrangement *notIns, unsigned int cellIndex, unsigned int newSimplexStart) {
// 
//         vector<unsigned int> queue;
//         vector<unsigned int> visited;
//         vector<unsigned int> inter;
//         unsigned int index = init - newSimplexStart;
//         unsigned int qSize;
//         bool discovered = false;
//         //here
//         visited.push_back(index);
//         queue.push_back(index);
//         Simplex S = trian.at(init);
//         trian.at(init).addToCL(h);
//         (*notIns).addToCL(true, h, init, cellIndex);
//         bool inList;
//         AdjGraph adjSub = adj.getSub(newSimplexStart);
//         //    adjSub.print();
//         qSize = queue.size();
//         while (qSize > 0) {
//             //get intersections
//             inter = adjSub.getInter(index);
//             //for each intersections
//             for (unsigned int i = 0; i < inter.size(); ++i) {
// 
//                 discovered = unIntIn(inter[i], visited);
//                 //
//                 if (!discovered) {
//                     //get that simplex
//                     S = trian.at(inter[i] + newSimplexStart);
//                     //do they intersect?
//                     inList = S.intersect(h);
//                     if (inList) {
//                         trian.at(inter[i] + newSimplexStart).addToCL(h);
//                     }
// 
//                     //add this to conflict list
//                     (*notIns).addToCL(inList, h, inter[i] + newSimplexStart, cellIndex);
//                     //we have visited this simplex?/
// 
//                     visited.push_back(inter[i]);
//                     //add to queue
//                     queue.push_back(inter[i]);
//                 }
//             }
// 
//             //get next simplex
//             index = queue.at(0);
//             //cout<<index<<"ind\n";
//             //remove this one from queue
//             queue.erase(queue.begin());
//             qSize = queue.size();
// 
//         }
// 
//     }

    bool unIntIn(unsigned int c, vector<unsigned int > vv) {

        bool in = false;
        unsigned int count = 0;
        while (!in && count < vv.size()) {


            in = vv[count] == c;
            count = count + 1;
        }

        return in;
    }
    
    vector<Polygon> getActiveFaces(int kLevel,Arrangement full){
        vector<Polygon> activeFaces;
        int planeNum,lev;
        Hyperplane h;
        for (unsigned int i = 0; i < faces.size(); ++i) {
            planeNum=faces[i].getPlane();
            if(planeNum>=0){
                h=full.getPlane(planeNum);
                lev=trian[0].getSideLevel(h);
                if(lev==kLevel)
                    activeFaces.push_back(faces[i]);
                
                }
            }
            return activeFaces;
        }
    

    vector<vector<vector<int >>> getFaceL() {

        vector<vector<vector<int >>> faceList;

        for (unsigned int i = 0; i < faces.size(); ++i) {
            faceList.push_back(faces[i].getVertList());

        }
        return faceList;
    }

    vector<vector<vector<double >>> getFaceCoord() {

        vector<vector<vector<double >>> faceList;

        for (unsigned int i = 0; i < faces.size(); ++i) {
            faceList.push_back(faces[i].getVertCoord());

        }
        return faceList;
    }

    void checkList(){
        bool tmp;
        bool tmp2;
      //  cout<<"the check\n";
       for (unsigned int i = 0; i < trian.size(); ++i) {
            for(unsigned int j = 0; j < trian.size(); ++j){
            if(i!=j){
                tmp=trian[i].isAdjacent(trian[j]);
                if(tmp){
                    tmp2=1==adj.get(i,j);
               if(!tmp2)
                        cout<<tmp2<<" ";
                  }
                else{
                    tmp2=0==adj.get(i,j);
                    if(!tmp2)
                        cout<<tmp2<<" ";
                  }
            }
        //    else
            //    cout<<1<<" ";
            
            }
       //     cout<<"\n";
        } 
    }

    void printTrian() {
        string tmp;
        //cout<<"num active simplices "<<active<<"\n";
        for (unsigned int i = 0; i < trian.size(); ++i) {
            cout << "#Simplex " << i << "\n";
            //              tmp = "s" + string::to_string(i) + "<-"
            trian[i].print("Simplex<-");
            cout << "\n";

        }

    }

    void printLevels() {
        for (unsigned int i = 0; i < trian.size(); ++i) {
            trian[i].printLevel();

        }
    }

    void printFaces() {

        for (unsigned int i = 0; i < faces.size(); ++i) {

            faces[i].print("f<-");
            cout << "\n\n";

        }

    }
    
    void printGraph(){
        adj.print();
    }

    void printCLS() {
        for (unsigned int i = 0; i < trian.size(); ++i) {
            cout << "Simplex: ";
            trian[i].print("");
            cout << "CL: ";
            trian[i].printCL();
            cout << "\n";
        }
        cout << "\n";

    }
};

