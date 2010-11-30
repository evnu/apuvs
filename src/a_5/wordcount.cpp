/*
 * =====================================================================================
 *
 *       Filename:  wordcount.cpp
 *
 *    Description:  APUVS Aufgabe 5 Map-Reduce
 *
 *        Version:  1.0
 *        Created:  27.11.2010 13:48:39
 *       Revision:  none
 *       Compiler:  gcc
 *
 * =====================================================================================
 */

#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <mpi.h>
#include <stdlib.h>

using namespace std;

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  Tokenize
 *  Description:  simple string tokenizer
 * =====================================================================================
 */
void Tokenize(const string &str, vector<string> &tokens){

    const string delimiters = " \"\n\t.,;:-+/?!()[]";

    string::size_type tokenBegin = str.find_first_not_of(delimiters, 0);
    string::size_type tokenEnd = str.find_first_of(delimiters, tokenBegin);

    while(string::npos != tokenBegin || string::npos != tokenEnd){
        //add token to vector
        tokens.push_back(str.substr(tokenBegin, tokenEnd - tokenBegin));
        //and find new token
        tokenBegin = str.find_first_not_of(delimiters, tokenEnd);
        tokenEnd = str.find_first_of(delimiters, tokenBegin);
    }
}

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  mapFile
 *  Description:  opens file and processes it line by line; emits vector of
 *  <word,1> maps
 * =====================================================================================
 */
<<<<<<< HEAD
void mapFile ( char* fileName, map<string,int> &outputMap )
=======
void mapFile ( char* fileName, multimap<string,int> &outputMap )
>>>>>>> a7c9fcc8b06cead0614b04d21d3f6fd5f5d474d3
{
    pair<map<string,int>::iterator,bool> ret;
    string line;
    ifstream file(fileName);
    if(file.is_open()){
        while( getline( file, line ) ){
            vector<string> token;
            Tokenize( line, token);
            
            for(vector<string>::iterator i = token.begin(); i != token.end(); i++){
                ret = outputMap.insert(pair<string, int>(*i, 1));
                if(!ret.second){
                    (*(ret.first)).second++;
                }
            }
        }
    }


    return ;
}		/* -----  end of function map  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  printMap
 *  Description:  just for debuging
 * =====================================================================================
 */
<<<<<<< HEAD
void printMap ( map<string, int> &toPrint)
=======
void printMap ( multimap<string, int> &toPrint)
>>>>>>> a7c9fcc8b06cead0614b04d21d3f6fd5f5d474d3
{
    map<string,int>::iterator it;
    for ( it=toPrint.begin() ; it != toPrint.end(); it++ )
            cout << (*it).first << " => " << (*it).second << endl;
    return ;
}		/* -----  end of function printMap  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  mapSize
 *  Description:  calculate size of map including \n for each key and value
 * =====================================================================================
 */
<<<<<<< HEAD
    int
mapSize ( map<string, int> toCount )
=======
int mapSize ( multimap<string, int> toCount )
>>>>>>> a7c9fcc8b06cead0614b04d21d3f6fd5f5d474d3
{
    map<string,int>::iterator it;
    int size = 0;
    for ( it=toCount.begin() ; it != toCount.end(); it++ ){
        size += (*it).first.length() * sizeof(char) + 1;
        size += sizeof((*it).second) + sizeof(char);
    }
    return size;
}		/* -----  end of function mapSize  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  serializeMap
 *  Description:  
 * =====================================================================================
 */
<<<<<<< HEAD
    void
serializeMap ( map<string, int> &toSerialize, string &serialized )
=======
void serializeMap ( multimap<string, int> &toSerialize, string &serialized )
>>>>>>> a7c9fcc8b06cead0614b04d21d3f6fd5f5d474d3
{
    map<string,int>::iterator it;
    for ( it=toSerialize.begin() ; it != toSerialize.end(); it++ ){
        serialized += (*it).first + "\n";
        serialized += "1\n";
        //serialized += (*it).second + "\n";
    }
    return ;
}		/* -----  end of function serializeMap  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  main
 *  Description:  checks cl-arguments and does all MPI stuff
 * =====================================================================================
 */

int main ( int argc, char *argv[] )
{
    if(argc < 2){
        cout	<< "Not enough arguments\nPlease specify a list of files to be wordcounted" << endl;
        exit(-2);
    }

    //initialize MPI stuff
    MPI::Init(argc, argv);

    int numPEs = MPI::COMM_WORLD.Get_size();
    int myID = MPI::COMM_WORLD.Get_rank();
    assert(numPEs <= argc - 1); //we should have at least as many files as PEs?!

    int length, rest;

    length = (argc - 1) / numPEs; //base chunk for every PE
    rest = (argc - 1) % numPEs; //rest of files to spread around

    //partition quick and quite dirty ;) but should work
    for(int i = 0; i < (myID + 1 <= rest ? length + 1 : length); i++){
        //cout << myID << ": i have file " << argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)] << endl; 

        // apply map
        map<string, int> countedWords;
        mapFile(argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)], countedWords);
        printMap( countedWords );
        //cout << "Mapsize is " << mapSize( countedWords ) << endl;
        string serialMap = "";
        serializeMap( countedWords, serialMap );
        //cout << serialMap << endl;
        // reduce
        // done
    }

    MPI::Finalize();
    return EXIT_SUCCESS;
}
/* ----------  end of function main  ---------- */

