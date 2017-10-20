#include <vector>
#include "Hyperplane.h"
#include "Simplex.h"
#include "Arrangement.h"
#include <cmath>
#include <iostream>
#ifndef __Polygon_INCLUDED__   // if x.h hasn't been included yet...
#define __Polygon_INCLUDED__   //   #define this so the compiler knows it has been included

using namespace std;

class Polygon {
    vector<Vertex > verts;
    unsigned int bv;
    vector<Simplex> tri;
    bool fake;

public:

    Polygon() {
    }

    Polygon(Vertex v1, Vertex v2, Vertex v3, Vertex v4, bool un) {
        verts.push_back(v1);
        verts.push_back(v2);
        verts.push_back(v3);
        verts.push_back(v4);
        calcBV();
        fake = un;
    }

    Polygon(Vertex v1, Vertex v2, Vertex v3, bool un) {
        verts.push_back(v1);
        verts.push_back(v2);
        verts.push_back(v3);
        calcBV();
        fake = un;
    }
    //must put vertices in order

    Polygon(vector<Vertex > ver, bool un) {
        verts = ver;
        fake = un;
        calcBV();
    };

    Polygon(vector<Vertex > ver, unsigned int b, bool un) {
        verts = ver;
        bv = b;
        fake = un;

    };

    //calculates the bv

    void calcBV() {
        double oldV = verts.at(0).getCoord().at(0);
        unsigned int index = 0;
        double curr;

        for (unsigned int i = 1; i < verts.size(); ++i) {

            //xval
            curr = verts.at(i).getCoord().at(0);
            //if lower xval
            if (curr < oldV) {
                oldV = curr;
                index = i;
            }//if same x val
            else if (curr == oldV) {
                //if new has lower y val
                if (verts.at(index).getCoord()[1] > verts.at(i).getCoord()[1]) {
                    oldV = curr;
                    index = i;
                }//if same y val
                else if (verts.at(index).getCoord()[1] == verts.at(i).getCoord()[1]) {
                    if (verts.at(index).getCoord()[2] > verts.at(i).getCoord()[2]) {
                        oldV = curr;
                        index = i;
                    }
                }
            }
        }

        bv = index;
    };

    void setVerts(vector<Vertex > newVert) {

        verts = newVert;
    }

    void setBv(unsigned int newBv) {

        bv = newBv;
    }

    bool isEdge(vector<Vertex> pair) {

        Vertex v1 = pair[0];
        Vertex v2 = pair[1];
        bool val = false;

        for (unsigned int i = 0; i < verts.size(); ++i) {

            //if v1 is curr
            if (v1.equals(verts[i])) {
                //is next or prev v2? we usually already checked prev....
                val = v2.equals(verts[next(i, verts.size())]);
                //if i =0 we didnt check prev
                if (!val && i == 0)
                    val = v2.equals(verts[verts.size() - 1]);
                if (val)
                    break;
            } else if (v2.equals(verts[i])) {
                //is next or prev v1? we usually already checked prev....
                val = v1.equals(verts[next(i, verts.size())]);
                //if i =0 we didnt check prev
                if (!val && i == 0)
                    val = v1.equals(verts[verts.size() - 1]);
                if (val)
                    break;
            }
        }

        return val;

    }

    unsigned int next(unsigned int k, unsigned int vecSize) {

        unsigned int val;
        if (k==vecSize - 1)
            val = 0;
        else
            val = k+1;
        return val;
    }

    unsigned int prev(unsigned int k, unsigned int max) {

        unsigned int val;
        if (k > 0)
            val = k - 1;
        else
            val = max - 1;
        return val;
    }


    //gets convex combo of points, if the weights are not in 0,1 it return -1

/*     double getAlpha(vector<double> v1, vector<double> v2, vector<double> cand) {
        double alpha;
        double alpha2;
        double alpha3;


        alpha = (cand[0] - v2[0]) / (v1[0] - v2[0]);
        alpha2 = (cand[1] - v2[1]) / (v1[1] - v2[1]);
        alpha3 = (cand[2] - v2[2]) / (v1[2] - v2[2]);
         if (alpha != alpha2 || alpha != alpha3 || alpha3 != alpha2 || alpha2 < 0 || alpha > 1) {
            alpha = -1;
        }// 

        return alpha;
    } */

    Vertex getbvV() {
        return verts.at(bv);
    }

    //find new simplices from just added vertices

    vector<Simplex> getNewS(vector<Vertex> justAdded) {

        vector<Simplex> s;
        unsigned int v1, v2;
        //for each vertex
        
    //    for (unsigned int i = 0; i < justAdded.size(); ++i) {
    //        justAdded[i].print(true);
    //    }
    //    cout<<"\n done\n \n";
        
        for (unsigned int i = 0; i < verts.size(); ++i) {
            //get indices
            if (verts[i].equals(justAdded[0]))
                v1 = i;
            else if (verts[i].equals(justAdded[1]))
                v2 = i;

        }
        //   cout<<"v1 and v2 and bv "<<v1<<" "<<v2<<" "<<bv<<"\n";
        //if one is the bv we re triangulate
        if (v2 == bv || v1 == bv) {

            s = triangulate();
        }//if second vertex is next
        else if (v2 == next(v1, verts.size())) {
            //add simplex between the vertex1 and vertex2 and bv
            if (v2 != bv && v1 != bv){
                s.push_back(Simplex(verts[bv], verts[v1], verts[v2], -1));
            }
            //add simplex if the prev vertex isnt the bv
            if (bv != prev(v1, verts.size()) && v1 != bv)
                s.push_back(Simplex(verts[bv], verts[v1], verts[prev(v1, verts.size())], -1));
            //add simplex if next from v2 is not bv
            if (bv != next(v2, verts.size()) && bv != v2)
                s.push_back(Simplex(verts[bv], verts[next(v2, verts.size())], verts[v2], -1));
        } 
        
        else if (v1 == next(v2, verts.size())) {
            //add simplex between the vertex1 and vertex2 and bv
            if (v1 != bv)
                s.push_back(Simplex(verts[bv], verts[v1], verts[v2], -1));
            //add simplex if the prev vertex v2 isn't the bv
            if (bv != prev(v2, verts.size()))
                s.push_back(Simplex(verts[bv], verts[v2], verts[prev(v2, verts.size())], -1));
            //add simplex if next from v1 is not bv
            if (bv != next(v1, verts.size()) && bv != v1)
                s.push_back(Simplex(verts[bv], verts[next(v1, verts.size())], verts[v1], -1));
        } 
        
        else {
            cout << "there is an error\n";
        }

        return s;
    }


    vector<Simplex> triangulate() {


        tri.clear();
        for (unsigned int i = 0; i < verts.size() - 2; ++i) {

            if (bv + i + 2 < verts.size())
                tri.push_back(Simplex(verts[bv], verts[bv + i + 1], verts[bv + i + 2], -1));
            else if (bv + i + 1 < verts.size())
                tri.push_back(Simplex(verts[bv], verts[bv + i + 1], verts[0], -1));
            else
                tri.push_back(Simplex(verts[bv], verts[(bv + i + 1) - verts.size()], verts[(bv + i + 2) - verts.size()], -1));
        }

        return tri;
    }

    //tells if a face is fake

    bool isFake() {

        return fake;
    }

    void printV(vector<unsigned int > v){
        cout<<"\n";
        for (unsigned int i = 0; i < v.size(); ++i) {
            cout<<" "<<v[i];
        }
        cout<<"\n";
    }
    //gets intersection points of plane and face, returns null if they don't intersect

    Polygon getFace(Hyperplane h, vector<Simplex > intersected, vector<Vertex > *newV, Vertex cellBV) {
        bool inter;
        //which vertices do we have in common with simplices?
        vector<unsigned int > indices;
        vector<unsigned int > neigh;
        Polygon newF;
        bool cont;
        unsigned int curr;
        vector<Vertex > verts2, verts3;
    //    cout<<"plane: "; 
    //    h.printP();
   //     for (unsigned int i = 0; i < intersected.size(); ++i)
     //       intersected[i].print("\n Simplex: \n");
        //does the polygon intersect the plane?
        for (unsigned int i = 0; i < intersected.size(); ++i) {
            //for each vertex in the simplex
            for (unsigned int j = 0; j < intersected[i].getVerts().size(); ++j) {
                //for each vertex in the face
                for (unsigned int k = 0; k< verts.size(); ++k) {

         //           cout<<"checking:\n ";
       //             verts[k].print(false);
                    
     //               cout<<"\n";
                    
                    
                    inter = verts[k].equals(intersected[i].getVerts()[j]);

                    //if the vertices are equal
                    //is the vertex is in the polygon?
                    if (inter) {
                        //if i is not already in the it intersects
                        if (notIn2(k, indices))
                            indices.push_back(k);
                    }
                }


                //             
            }
        }

                //if there are more than 2 vertices then it might intersect in an edge
                // 2 since we don't want the bv face....
   //     print("");
        if(indices.size() >1) {
            //which simplex?
            //get new vertices if intersect in edge
         //   printV(indices);
            intersectEdge(h, newV, indices, &neigh);
        }

     
        // //modify into 2 faces....
        // //while we have not hit prev vertex

        //if intersect, continue
        cont = (*newV).size() > 1;
		if((*newV).size() ==1){
			printV(neigh);
			printV(indices);
			for (unsigned int i = 0; i < intersected.size(); ++i)
				intersected[i].print("\n Simplex<-");
			cout<<"\n";
			print("f<-");
		}
		
//problem here August 18
        if (cont) {

            //put in right order
            if (next(neigh[0], verts.size()) == neigh[1]) {
                curr = neigh[1];
                neigh[1] = neigh[0];
                neigh[0] = curr;
            } 
            
            else
                curr = neigh[0];

            verts2.push_back((*newV)[0]);
            verts2.push_back(verts[curr]);

            //if not triangle keep going...
            cont = curr != neigh[2] && curr != neigh[3];
            //infinite loop
            while (cont) {

                curr = next(curr, verts.size());
                verts2.push_back(verts[curr]);

                cont = curr != neigh[2] && curr != neigh[3];
            }
            //                

            verts2.push_back((*newV)[1]);

            cont = true;
            verts3.push_back((*newV)[1]);

            while (cont) {

                curr = next(curr, verts.size());
                verts3.push_back(verts[curr]);
                cont = curr != neigh[1];
            }

            verts3.push_back((*newV)[0]);

            newF = Polygon(verts3, fake);


            //if this face contains bottom vertex
            if (cellBV.equals(Polygon(verts2, fake).getbvV())) {
                //newF=Polygon(verts3,fake);
                verts = verts2;
            }//if verts 3 has bottom vertex
            else if (cellBV.equals(newF.getbvV())) {
                newF = Polygon(verts2, fake);
                verts = verts3;
            }
            //if the plane separates the new face and the bv
            else if (getWeight2(h, newF.verts[1].getCoord(), cellBV.getCoord()) < 1 && getWeight2(h, newF.verts[1].getCoord(), cellBV.getCoord()) > 0)
                verts = verts2;
            //
            else {
                newF = Polygon(verts2, fake);
                verts = verts3;
            }

      //   cout<<bv<<" "<<newF.bv<<"\n";
		 calcBV();            
        }
        return newF;
    }

    bool adjacent(unsigned int i1, unsigned int i2) {
  
        return i2 == next(i1,verts.size()) || i1 == next(i2,verts.size());

    }

    void intersectEdge(Hyperplane h, vector<Vertex > *newV, vector<unsigned int > indices, vector<unsigned int > *neigh) {

        vector<double > v1;
        vector<double > v2;
        double x, y, z,Z1,Z2;
        double alpha;
        bool unbounded;
        vector<int> posit;
        Vertex tmp;
        bool oppSides;
        

        //for each intersection line
        for (unsigned int k = 0; k < indices.size()-1; ++k) {
            for (unsigned int i = k+1; i < indices.size(); ++i) {

                //if beside each other
                if (adjacent(indices[i], indices[k])) {


              //      cout<<i<<" "<<k<<"kjhk\n";
                  
                    v1 = verts[indices[i]].getCoord();
                    v2 = verts[indices[k]].getCoord();
                    Z1=h.getZ(v1[0],v1[1]);
                    Z2=h.getZ(v2[0],v2[1]);
                    oppSides=((v1[2]<Z1)&&(v2[2]>Z2))||((v2[2]<Z2)&&(v1[2]>Z1));
                    
            //        cout<<Z1<<" "<<Z2<<" "<<oppSides<<"op\n";
                    //if in 0,1, there is edge intersection
                     //create new vertex
                    if(oppSides) {
                        //get coordinates
                        alpha = getWeight(h, indices[i], indices[k]);
                        
                        
                        //update position of new vertex
                        posit=verts[indices[k]].getCommon(verts[indices[i]]);
                        posit.push_back(h.pos());
                       
                        //printV(posit);

                        
                       //bounded?
                        unbounded = (verts[indices[i]].isUnbounded())&&(verts[indices[k]].isUnbounded());
                        
                        //coords
                        x = alpha * v2[0]+(1 - alpha) * v1[0];
                        y = alpha * v2[1]+(1 - alpha) * v1[1];
                        z = alpha * v2[2]+(1 - alpha) * v1[2];
                        
                        //tmp is the new vertex
                        tmp = Vertex(x, y, z, unbounded, posit);

             //           ins=!in(tmp, (*newV));
             //           cout<<ins<<" ins \n";

             //           if (ins) {
                            (*newV).push_back(tmp);
                            (*neigh).push_back(indices[i]);
                            (*neigh).push_back(indices[k]);
            //            }

                    }
                }

            }
        }

    }

    bool isAdjacent(Polygon s) {
        bool value = false;
        int count = 0;

            //find 2 verts that match
            for (unsigned int i = 0; i < verts.size(); ++i) {
                for (unsigned int j = 0; j < s.verts.size(); ++j) {
                    if (verts[i].equals(s.verts[j]))
                        ++count;
                }
            }
            value = count >= 2;
        
        return value;
    }
    bool in(Vertex c, vector<Vertex > vv) {

        bool in = false;
        unsigned int count = 0;
        while (!in && count < vv.size()) {


            in = vv[count].equals(c);
            count = count + 1;
        }

        return in;
    }

    bool notIn2(unsigned int c, vector<unsigned int > vv) {

        bool in = false;
        for(unsigned int i=0; i< vv.size();++i) {

            in = vv[i] == c;
            if(in)
                break;
        }

        return !in;
    }

	bool isRelevant(){
    	bool rel=false;
    	for(unsigned int i = 0; i < verts.size(); ++i){
	    rel=!verts[i].isUnbounded();
	    if(rel)
	        break;
	    }
	    return rel;
	}
	int getLevel(Arrangement full){
		int level;
		vector<double> avg;
		avg.assign(3,0);
		//if(isRelevant())
		//{
		for (unsigned int i = 0; i < verts.size(); ++i) {
                avg[0] = avg[0] + verts.at(i).getCoord()[0] / verts.size();
                avg[1] = avg[1] + verts.at(i).getCoord()[1]/ verts.size();
                avg[2] = avg[2] +verts.at(i).getCoord()[2] / verts.size();
            }
			
		level=full.getLevel(avg,getPlane()).getValue();
		//}
		//else
    	//	level=-1;
		return level;
	}

	int getPlane(){
		vector<vector<int >> vertList=getVertList();
		vector<int > home=vertList[0];
		vector<int > curr1=vertList[1];
		vector<int > curr2=vertList[2];
		int common=-10;
		for(unsigned int i=0;i<home.size();++i){
			if(In2(home[i],curr1)&&In2(home[i],curr2)){
				common=home[i];
				break;
			}
		}
		return common;
	}

	bool In2( int c, vector<int > vv) {

        bool in = false;
        for(unsigned int i=0; i< vv.size();++i) {

            in = vv[i] == c;
            if(in)
                break;
        }

        return in;
    }
    //return alpha * v2 + (1-alpha) * v1

    double getWeight(Hyperplane h, unsigned int i1, unsigned int i2) {

        vector<double > v1 = verts[i1].getCoord();
        vector<double > v2 = verts[i2].getCoord();

      //  verts[i1].print(false);
      //  verts[i2].print(false);       
        
        return getWeight2(h, v1, v2);
    }

    double getWeight2(Hyperplane h, vector<double > v1, vector<double > v2) {


        vector<double > planeCoeff = h.getCoef();
        vector<double > diff;
      //  double mult = 1;
        double alpha, a2;


        for (unsigned int i = 0; i < 3; ++i) {
            diff.push_back(v1[i] - v2[i]);
        }


        alpha = dotProd(planeCoeff, v1) + planeCoeff[3];
        a2 = dotProd(diff, planeCoeff);
        alpha=alpha/a2;
 //       cout<<"alpha "<<alpha<<" ";
        return alpha;
    }

    double dotProd(vector<double > v1, vector<double > v2) {

        return v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2];

    }
    
    vector<vector<int >> getVertList(){
     
        vector<vector<int >> vertList;
        vector<int > curr;
        int num=0;
        for(unsigned int i=0;i<verts.size();++i){
            curr=verts[i].getPlanesI();
           // num=(int)(curr[0]>0)+(int)(curr[1]>0)+(int)(curr[2]>0);
         //   if(num>=2)
                vertList.push_back(curr);            
        }
        return vertList;
    }
    
    vector<vector<double >> getVertCoord(){
     
        vector<vector<double >> vertList;
        
        for(unsigned int i=0;i<verts.size();++i){
              vertList.push_back(verts[i].getCoord());            
        }
        return vertList;
    }

    



    bool equals(Polygon c) {

        bool eq;
        eq = verts.size() == c.verts.size();
        bool tmp;
        unsigned int start;

        if (eq) {
            for (unsigned int i = 0; i < verts.size(); ++i) {
                tmp = verts[i].equals(c.verts[0]);
                if (tmp) {
                    start = i;
                    break;
                }
            }
            if (tmp) {
                for (unsigned int i = 0; i < verts.size(); ++i) {
                    if (i + start < verts.size())
                        tmp = verts[i + start].equals(c.verts[i]);
                    else
                        tmp = verts[i + start - verts.size()].equals(c.verts[i]);

                    if (!tmp) {
                        eq = false;
                        break;
                    }
                }
            } 
            else
                eq = false;

        }
        return eq;
    }

    void print(string begin) {
        cout << begin << "rbind(";
        for (unsigned int i = 0; i < verts.size(); ++i) {

            verts[i].print(i == verts.size() - 1);
            //	    cout<<"\nbv is "<<bv<<" fake? "<<fake<<"\n";

        }
        cout << ")";

        cout << "\nbv= " << bv;
    }

};
//int main(void){return 0;};
#endif 