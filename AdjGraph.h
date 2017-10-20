#include <string>
#include <vector>
#include "Simplex.h"
using namespace std;

class AdjGraph {
    vector<vector<unsigned int> > graph;

public:
    //        AdjGraph(int k){
    //        	vector<int> tmp={1};
    //            graph.push_back(tmp);
    //        };

    AdjGraph(unsigned int size) {

        vector<unsigned int> emptyRow;
        emptyRow.assign(size, 0);

        for (unsigned int i = 0; i < size; ++i) {
            graph.push_back(emptyRow);
        }

    }

    AdjGraph(){
    };

    AdjGraph(vector<vector<unsigned int> > g) {
        graph = g;
    };


    vector<vector<unsigned int> > getGraph() {

        return graph;
    }
    
    void dummy(){
    cout<<"dummy\n";
    }

    void set(unsigned int val, unsigned int i, unsigned int j) {

        graph[i][j] = val;
        graph[j][i] = val;

    }
    
    unsigned int get(unsigned int i, unsigned int j) {

        return graph[i][j];

    }

    vector<unsigned int> getRow(unsigned int i) {
        return graph.at(i);
    }

    unsigned int getRowSum(unsigned int i) {

        unsigned int sum = 0;
        vector<unsigned int> row=graph[i];
        for (unsigned int j=0; j < row.size(); ++j)
            sum=sum + graph[i][j];
        return sum;
    }

    void rmvNode(unsigned int ind) {
        graph.erase(graph.begin() + ind);
        for (unsigned int i = 0; i < graph.size(); ++i) {

            graph.at(i).erase(graph.at(i).begin() + ind);

        }

    }

    void extend(unsigned int by) {
        unsigned int n = graph.size();
        vector<unsigned int> tmp;
        tmp.assign(n + by, 0);
        for (unsigned int i = 0; i < by; ++i) {

            graph.push_back(tmp);
            graph[i + n][i + n] = 1;
        }

        for (unsigned int i = 0; i < n; ++i) {
            for (unsigned int j = 0; j < by; ++j) graph[i].push_back(0);
        }

    }

    void addNode(vector<unsigned int> node) {
        graph.push_back(node);
        for (unsigned int i = 0; i < graph.size(); ++i) {

            graph.at(i).push_back(node.at(i));

        }
    }

    void insertNode(vector<unsigned int> node, unsigned int pos) {
        graph.insert(graph.begin() + pos, node);
        for (unsigned int i = 0; i < graph.size(); ++i) {

            graph.at(i).insert(graph.at(i).begin() + pos, node.at(i));

        }
    }
    //returns the adjacent relations at row index

    vector<unsigned int> getInter(unsigned int index) {
        vector<unsigned int> inter;
        for (unsigned int i = 0; i < graph[index].size(); ++i) {
            if (i != index && graph[index][i] == 1) {
                inter.push_back(i);
            }
        }
        return inter;
    }
	
	
    //returns the adjacent relations at row index above a certain index

    vector<unsigned int> getInterAbove(unsigned int index, unsigned int above) {
        vector<unsigned int> inter;
        for (unsigned int i = above + 1; i < graph[index].size(); ++i) {
            if (i != index && graph[index][i] == 1) {
                inter.push_back(i);
            }
        }
        return inter;
    }
    //gets sub graph after certain index

    AdjGraph getSub(unsigned int row) {
        AdjGraph sub=AdjGraph(graph.size() - row);
        
       // vector<unsigned int> tmp;
        //set size of new graph
        //tmp.assign(graph.size() - row, 0);
       // sub.graph.assign(graph.size() - row, tmp);

        for (unsigned int i = row; i < graph.size(); i++) {
            for (unsigned int j = row; j < graph.size(); j++) {
                sub.graph[i - row][j - row] = graph[i][j];
               // sub.graph[j - row][i - row] = graph[i][j];
               // sub.graph[j - row][i - row] = graph[j][i];
            }
        }
       // sub.printDims();
        return sub;

    }
    
    void printDims(){
        cout<<graph.size();
        for (unsigned int i = 0; i < graph.size(); ++i) {
            cout<<"\n This row is size "<<graph[i].size()<<" "<<i;
            
            cout<<"\n";
        }
    }
    
   void printRow(unsigned int row) {
       cout<<"\n";
        for (unsigned int i = 0; i < graph.size(); ++i) {

                cout << graph[row][i] << " ";
            
        }


    }


    void print() {
        //printDims();
        for (unsigned int i = 0; i < graph.size(); ++i) {
            cout << "\n";
            for (unsigned int j = 0; j < graph[i].size(); ++j) {

                cout << graph[i][j] << " ";
            }
        }


    }

};
