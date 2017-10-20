#include <string>
#include <vector>
#include "Hyperplane.h"
#include "Cell.h"
#include "Arrangement.h"


using namespace std;

class Canon {
    vector<Cell> cells;
    AdjGraph adjGraph;
public:

    Canon() {
    };

    Canon(Arrangement *out, int kLevel) {
        // cout<<"here:";
        makeInitCells();
        vector<unsigned int> r1 = {1};
        vector<vector<unsigned int> > g = {r1};
        adjGraph = AdjGraph(g);
        initCL(out);
        initLevels((*out), kLevel);

    };
/*     Canon(Arrangement *in, Arrangement *out, Arrangement full, int kLevel) {
        // cout<<"here:";
        makeInitCells(&((*in).getPlanes().at(0)));

        vector<unsigned int> r1 = {1, 1};
        vector<unsigned int> r2 = {1, 1};
        vector<vector<unsigned int> > g = {r1, r2};
        //             for(unsigned int i=0;i<g.size();++i){
        //             	cout<<g[i][g.size()-1]<<" ";
        // 			}
        adjGraph = AdjGraph(g);
        initCL(out);
        initLevels(full, kLevel);

    }; */

    //makes 1 cell;
    void makeInitCells() {

        
        //  Hyperplane *rightAbove, *leftAbove;
        double infin = 1000000;
        //xy high high...
       vector<Vertex > verts;
        //xyz high or low, four intersections with box
        Vertex hhh, hhl, hlh, lhh, llh, hll, lhl, lll;
        Cell temp;
        vector<Polygon> faces;
        // unsigned int n;
        vector<int> positions;
        //right side, is which plane....


        //each pos has a meaning for pos on the box
        //-1 to -6, x=-inf,y=-inf,z=-inf,...,z=inf
        positions = {-4, -5, -6};
        hhh = Vertex(infin+0.025, infin+0.87602, infin+0.07832, true, positions);
        positions = {-4, -5, -3};
        hhl = Vertex(infin+0.023, infin+0.802, -infin+0.03872, true, positions);
        positions = {-4, -2, -6};
        hlh = Vertex(infin+0.027, -infin+0.4702, infin+0.03782, true, positions);
        positions = {-1, -5, -6};
        lhh = Vertex(-infin+0.97, infin+0.062, infin+0.732, true, positions);
        positions = {-1, -2, -3};
        lll = Vertex(-infin+0.02, -infin+0.402, -infin+0.07832, true, positions);
        positions = {-1, -2, -6};
        llh = Vertex(-infin+0.0452, -infin+0.052, infin+83.02, true, positions);
        positions = {-1, -5, -3};
        lhl = Vertex(-infin+0.042, infin+0.024, -infin+0.07382, true, positions);
        positions = {-4, -2, -3};
        hll = Vertex(infin+1.02, -infin+0.026, -infin+0.078372, true, positions);


    //z high plane
        verts={hhh,lhh,llh,hlh};
        faces.push_back(Polygon(verts, true));
        verts.clear();
    //z low plane    
        verts={lll,lhl,hhl,hll};
        faces.push_back(Polygon(verts, true));
        verts.clear();

    //x high plane
        verts={hhh, hlh, hll, hhl};
        faces.push_back(Polygon(verts, true));
        verts.clear();

    //x low plane
        verts={lhh, lhl, lll, llh};
        faces.push_back(Polygon(verts, true));
        verts.clear();
        
    //y high plane
        verts={hhh, lhh, lhl, hhl};
        faces.push_back(Polygon(verts, true));
        verts.clear();

    //y low plane
        verts={lll, llh, hlh, hll};
        faces.push_back(Polygon(verts, true));
        verts.clear();
        
        temp = Cell(faces, true);
        cells.push_back(temp);


    }
    
    //makes initial 2 cells

/*     void makeInitCells(Hyperplane *p1) {

        //  Hyperplane *rightAbove, *leftAbove;
        double infin = 1000000;
        //xy high high...
        double curr;
        vector<Vertex > verts;
        //xyz high or low, four intersections with box
        Vertex hhh, hhl, hlh, lhh, llh, hll, lhl, lll;
        Cell temp;
        vector<Polygon> faces;
        Polygon realFace;
        // unsigned int n;
        vector<vector<double > > tmp;
        vector<double > tmp1;
        vector<int> positions;
        //right side, is which plane....


        //each pos has a meaning for pos on the box
        //-1 to -6, x=-inf,y=-inf,z=-inf,...,z=inf
        positions = {-4, -5, -6};
        hhh = Vertex(infin, infin, infin, true, positions);
        positions = {-4, -5, -3};
        hhl = Vertex(infin, infin, -infin, true, positions);
        positions = {-4, -2, -6};
        hlh = Vertex(infin, -infin, infin, true, positions);
        positions = {-1, -5, -6};
        lhh = Vertex(-infin, infin, infin, true, positions);
        positions = {-1, -2, -3};
        lll = Vertex(-infin, -infin, -infin, true, positions);
        positions = {-1, -2, -6};
        llh = Vertex(-infin, -infin, infin, true, positions);
        positions = {-1, -5, -3};
        lhl = Vertex(-infin, infin, -infin, true, positions);
        positions = {-4, -2, -3};
        hll = Vertex(infin, -infin, -infin, true, positions);

        //goes hh, hl, ll ,hh
        for (unsigned int i = 0; i < 4 && verts.size() < 4; ++i) {
            if (i == 0) {
                tmp1 = {infin, infin, 0};
                curr = (*p1).getZ(infin, infin);
                positions = {-4, -5, (*p1).pos()};
            } else if (i == 1) {
                tmp1 = {-infin, infin, 0};
                curr = (*p1).getZ(-infin, infin);
                positions = {-1, -5, (*p1).pos()};
            } else if (i == 2) {
                tmp1 = {-infin, -infin, 0};
                curr = (*p1).getZ(-infin, -infin);
                positions = {-1, -2, (*p1).pos()};
            } else if (i == 3) {
                tmp1 = {infin, -infin, 0};
                curr = (*p1).getZ(infin, -infin);
                positions = {-4, -2, (*p1).pos()};
            }
            if (curr < infin && curr>-infin) {
                verts.push_back(Vertex(tmp1[0], tmp1[1], curr, true, positions));
                tmp.push_back(tmp1);
            }

        }

        //goes hh, hl, lh , ll
        //goes hh, hl, ll ,hh
        for (unsigned int i = 0; i < 4 && verts.size() < 4; ++i) {
            if (i == 0) {
                tmp1 = {infin, 0, infin};
                curr = (*p1).getY(infin, infin);
                positions = {-4, (*p1).pos(), -6};
            } else if (i == 1) {
                tmp1 = {-infin, 0, infin};
                curr = (*p1).getY(-infin, infin);
                positions = {-1, (*p1).pos(), -6};
            } else if (i == 2) {
                tmp1 = {-infin, 0, -infin};
                curr = (*p1).getY(-infin, -infin);
                positions = {-1, (*p1).pos(), -3};
            } else if (i == 3) {
                tmp1 = {infin, 0, -infin};
                curr = (*p1).getY(infin, -infin);
                positions = {-4, (*p1).pos(), -3};
            }
            if (curr < infin && curr>-infin) {
                verts.push_back(Vertex(tmp1[0], curr, tmp1[2], true, positions));
                tmp.push_back(tmp1);

            }

        }

        //goes hh, hl, lh , ll
        //goes hh, hl, ll ,hh
        for (unsigned int i = 0; i < 4 && verts.size() < 4; ++i) {
            if (i == 0) {
                tmp1 = {0, infin, infin};
                curr = (*p1).getX(infin, infin);
                positions = {(*p1).pos(), -5, -6};
            } else if (i == 1) {
                tmp1 = {0, -infin, infin};
                curr = (*p1).getX(-infin, infin);
                positions = {(*p1).pos(), -2, -6};
            } else if (i == 2) {
                tmp1 = {0, -infin, -infin};
                curr = (*p1).getX(-infin, -infin);
                positions = {(*p1).pos(), -2, -3};
            } else if (i == 3) {
                tmp1 = {0, infin, -infin};
                curr = (*p1).getX(infin, -infin);
                positions = {(*p1).pos(), -5, -3};
            }
            if (curr < infin && curr>-infin) {
                verts.push_back(Vertex(curr, tmp1[1], tmp1[2], true, positions));
                tmp.push_back(tmp1);
            }

        }


        realFace = Polygon(verts, false);



        //if plane cuts along yz, assume all verts are this...

        if (tmp[0][0] == 0) {

            faces.push_back(Polygon(hhh, hhl, hll, hlh, true));
            faces.push_back(realFace);
            faces.push_back(Polygon(hhh, hhl, verts[3], verts[0], true));
            faces.push_back(Polygon(hhh, hlh, verts[1], verts[0], true));
            faces.push_back(Polygon(hll, hhl, verts[3], verts[2], true));
            faces.push_back(Polygon(hll, hlh, verts[1], verts[2], true));
            temp = Cell(faces, true);
            cells.push_back(temp);
            faces.clear();

            faces.push_back(Polygon(lhh, lhl, lll, llh, true));
            faces.push_back(realFace);
            faces.push_back(Polygon(lhh, lhl, verts[3], verts[0], true));
            faces.push_back(Polygon(lhh, llh, verts[1], verts[0], true));
            faces.push_back(Polygon(lll, lhl, verts[3], verts[2], true));
            faces.push_back(Polygon(lll, llh, verts[1], verts[2], true));
            temp = Cell(faces, true);
            cells.push_back(temp);
        } else if (tmp[0][1] == 0) {

            faces.push_back(Polygon(hhh, hhl, lhl, lhh, true));
            faces.push_back(realFace);
            faces.push_back(Polygon(hhh, hhl, verts[3], verts[0], true));
            faces.push_back(Polygon(hhh, lhh, verts[1], verts[0], true));
            faces.push_back(Polygon(lhl, hhl, verts[3], verts[2], true));
            faces.push_back(Polygon(lhl, lhh, verts[1], verts[2], true));

            temp = Cell(faces, true);
            cells.push_back(temp);
            //            temp.printFaces();
            faces.clear();

            faces.push_back(Polygon(hlh, hll, lll, llh, true));
            faces.push_back(realFace);
            faces.push_back(Polygon(hlh, hll, verts[3], verts[0], true));
            faces.push_back(Polygon(hlh, llh, verts[1], verts[0], true));
            faces.push_back(Polygon(lll, hll, verts[3], verts[2], true));
            faces.push_back(Polygon(lll, llh, verts[1], verts[2], true));
            temp = Cell(faces, true);
            cells.push_back(temp);
        } else if (tmp[0][2] == 0) {

            faces.push_back(Polygon(hhh, lhh, llh, hlh, true));
            faces.push_back(realFace);
            faces.push_back(Polygon(hhh, lhh, verts[3], verts[0], true));
            faces.push_back(Polygon(hhh, hlh, verts[1], verts[0], true));
            faces.push_back(Polygon(llh, lhh, verts[3], verts[2], true));
            faces.push_back(Polygon(llh, hlh, verts[1], verts[2], true));
            temp = Cell(faces, true);
            cells.push_back(temp);
            faces.clear();

            faces.push_back(Polygon(hhl, lhl, lll, hll, true));
            faces.push_back(realFace);
            faces.push_back(Polygon(hhl, lhl, verts[3], verts[0], true));
            faces.push_back(Polygon(hhl, hll, verts[1], verts[0], true));
            faces.push_back(Polygon(lll, lhl, verts[3], verts[2], true));
            faces.push_back(Polygon(lll, hll, verts[1], verts[2], true));
            temp = Cell(faces, true);
            cells.push_back(temp);
        } 
        else
            cout << "error \n";
    }

     */
    
    void initCL(Arrangement *out) {

        vector < vector < vector<bool> > > newCL;
        vector < vector<bool> > tmp;
        unsigned int n = (*out).size();
        //for each plane
        for (unsigned int i = 0; i < n; ++i) {
            //for each cell init list
            for (unsigned int j = 0; j < cells.size(); ++j) {
                tmp.push_back(cells.at(j).initCL((*out).getPlane(i)));
            }
            newCL.push_back(tmp);
            tmp.clear();
        }

        (*out).setList(newCL);
        // (*out).printCL();
    };

    Cell getFirstCell() {

        return cells.at(0);

    };



	void printLevels(){
		cout<<"Print Levels ";
		for(unsigned int i = 0; i < cells.size(); ++i) {
               cells[i].printLevels();
			cout << "\n";

        }
	}

    //add indices;

  void retri(Hyperplane p, vector <vector<bool> > oldCL, Arrangement *notIns,Arrangement full, int kLevel) {
        //change increment
        //NEW CELL IS FIRST
        unsigned int n = cells.size();
        unsigned int n2;
        //changed cells
        vector<unsigned int> S_indices;
        Cell tmp;

        // 		                   split cells in 2

        //for each cell
        for (unsigned int i = 0; i < n; ++i) {

            n2 = oldCL.at(i).size();

            for (unsigned int j = 0; j < n2; ++j) {
                //if the cell is intersected by the hyperplane
                if (oldCL[i][j]) {
                    //split cells in 2
                    //          cells.at(i).printFaces();
                    tmp = cells.at(i).split(p, j, oldCL.at(i), notIns,full, i, kLevel);
                    cells.push_back(tmp);
                    //changed cells
                    S_indices.push_back(i);
                    S_indices.push_back(cells.size() - 1);
                    break;
                };
            };
        };
        //update adj graph
        updateGraph(S_indices);
    };


  void updateGraph(vector<unsigned int> S) {
        vector<unsigned int> intersections;
        unsigned int extension = S.size() / 2;
    //    unsigned int tmppp;
        adjGraph.extend(extension);
        bool tmp;
        
        for (unsigned int i = 0; i < extension; ++i) {
            //get the cells surrounding this one previously
            intersections = adjGraph.getInter(S[i * 2]);
  //          tmp=intersections.size();
            //check if we still intersect these cells
            for (unsigned int j = 0; j < intersections.size(); ++j) {
                //are these two cells adjacent?
                tmp = cells[S[i * 2]].isAdj(cells[intersections[j]]);
                if (tmp)
                    adjGraph.set(1, intersections[j], S[i * 2]);
                    //if not adjacent check new cell
                else{
                    tmp = cells[S[i * 2] + 1].isAdj(cells[intersections[j]]);
                    if (tmp)
                        adjGraph.set(1, intersections[j], S[i * 2 + 1]);
                }

            }

        }

    };

    void removeInactive(Arrangement *notIns) {
        unsigned int count = 0;
        for (unsigned int i = 0; i < cells.size(); ++i) {
          //  cells[count].printFaces();


            if (cells.at(count).getActive() == 0) {			
                cells.erase(cells.begin() + count);
                adjGraph.rmvNode(count);
                (*notIns).removeCellFromCL(count);
            }
            else
                ++count;
        }
    }

    void initLevels(Arrangement full, int kLevel) {
        for (unsigned int i = 0; i < cells.size(); ++i) {

            cells[i].initLevels(full, kLevel);

        }
    };


    vector<vector<vector<int >>> finalTrim(int kLevel,Arrangement full){
       
		   vector<Polygon> faces;
		   vector<vector<vector<int >>> faceLabels;
		   int currLevel;
           for (unsigned int i = 0; i < cells.size(); ++i) {
			   faces=cells[i].getFaces();
			    for (unsigned int j = 0; j < faces.size(); ++j) {
					currLevel=faces[j].getLevel(full);
					if(currLevel==kLevel)
						faceLabels.push_back(faces[j].getVertList());
				}
          }
		return faceLabels;
     }
     
    vector<vector<vector<int >>> finalTrimEff(int kLevel,Arrangement full){
       
		   vector<Polygon> faces;
		   vector<vector<vector<int >>> faceLabels;
		   int currLevel;
        for (unsigned int i = 0; i < cells.size(); ++i) {
			   faces=cells[i].getActiveFaces(kLevel,full);
			    for (unsigned int j = 0; j < faces.size(); ++j) {
    			    if(notIn(faceLabels,faces[j].getVertList()))
	    					faceLabels.push_back(faces[j].getVertList());
				    }
          }
		    return faceLabels;
     }
     
    bool notIn(vector<vector<vector<int >>> list, vector<vector<int >> element){
     
         bool in=false;
     
        for (unsigned int i = 0; i < list.size(); ++i) {
            in=list[i]==element;
            if(in)
                break;
        }
     
        return !in;
     
     }

    ///takes the new cells in pairs;

    

    vector<vector<vector<int >>> getFaceList() {
        vector<vector<vector<int >>> faces;
        vector<vector<vector<int >>> tmp;
        for (unsigned int i = 0; i < cells.size(); ++i) {
            tmp = cells[i].getFaceL();
            for (unsigned int j = 0; j < tmp.size(); ++j) {
                faces.push_back(tmp[j]);
            }
        }
        return faces;
    }

    vector<vector<vector<double >>> getFaceCoord() {
        vector<vector<vector<double >>> faces;
        vector<vector<vector<double >>> tmp;
        for (unsigned int i = 0; i < cells.size(); ++i) {
            tmp = cells[i].getFaceCoord();
            for (unsigned int j = 0; j < tmp.size(); ++j) {
                faces.push_back(tmp[j]);
            }
        }
        return faces;
    }
    
    vector<vector<vector<double >>> finalTrim2(int kLevel,Arrangement full){
       
		   vector<Polygon> faces;
		   vector<vector<vector<double >>> faceLabels;
		   int currLevel;
           for (unsigned int i = 0; i < cells.size(); ++i) {
			   faces=cells[i].getFaces();
			    for (unsigned int j = 0; j < faces.size(); ++j) {
					currLevel=faces[j].getLevel(full);
					if(currLevel==kLevel)
						faceLabels.push_back(faces[j].getVertCoord());
				}
          }
		return faceLabels;
       }



    void print() {

        for (unsigned int i = 0; i < cells.size(); ++i) {
            cout << "#Cell Number " << i << "\n";
            cells[i].printTrian();
            cout << "\n\n";

        }

    }
    
    void printCLS() {

        for (unsigned int i = 0; i < cells.size(); ++i) {
            cout << "#Cell Number " << i << "\n";
            cells[i].printCLS();
            cout << "\n";
        }
        cout << "\n\n";

    }
    
    void printGraphs() {

        for (unsigned int i = 0; i < cells.size(); ++i) {
            cout << "#Cell Number " << i << "\n";
            cells[i].printGraph();
            cout << "\n";
        }
        cout << "\n\n";

    }
    
   void checkList() {

        for (unsigned int i = 0; i < cells.size(); ++i) {
    //        cout << "#Cell Number " << i << "\n";
            cells[i].checkList();
       //     cout << "\n";
        }

    }

    void printFaces() {

        for (unsigned int i = 0; i < cells.size(); ++i) {
            cout << "#Cell Number " << i << "\n";
            cells[i].printFaces();
            cout << "\n\n\n";

        }

    }

    void printGraph() {

        adjGraph.print();

    }


};