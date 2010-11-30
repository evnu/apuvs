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
#include <cctype>
#include <mpi.h>
#include <stdlib.h>
#include <string>

#define NUMBEROFDIGITSINANINTEGER 11

using namespace std;

// prototypes
string toLower (string);

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
void mapFile ( char* fileName, map<string,int> &outputMap )
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
		// TODO Throw error if file not opened

    return ;
}		/* -----  end of function map  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  printMap
 *  Description:  just for debuging
 * =====================================================================================
 */
void printMap ( map<string, int> &toPrint)
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
int mapSize ( map<string, int> toCount )
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
string serializeMap ( map<string, int> &toSerialize )
{
		char * buf = new char[NUMBEROFDIGITSINANINTEGER];//calloc (NUMBEROFDIGITSINANINTEGER, sizeof(char));
		string serialized;
    for ( map<string,int>::iterator it=toSerialize.begin(); 
					it != toSerialize.end(); it++ )
		{
        serialized += (*it).first + "\n";
				sprintf (buf, "%d\n", (*it).second);
        serialized += buf;
    }

		delete[] buf;
		return serialized;
}		/* -----  end of function serializeMap  ----- */

int wordToPE(string word, int numPEs){
	char first = *(word.c_str());
	return (first - 'a') % numPEs;
}


char convertMe (char c) {
	return tolower (c);
}

string toLower (string str) {
	std::transform (str.begin (), str.end (), str.begin (), convertMe);
	return str;
}

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

    map<string, int> countedWords;
    int numPEs = MPI::COMM_WORLD.Get_size();
    int myID = MPI::COMM_WORLD.Get_rank();
    //assert(numPEs <= argc - 1); //we should have at least as many files as PEs?!

    int length, rest;

    length = (argc - 1) / numPEs; //base chunk for every PE
    rest = (argc - 1) % numPEs; //rest of files to spread around

    //partition quick and quite dirty ;) but should work
    for(int i = 0; i < (myID + 1 <= rest ? length + 1 : length); i++){
        //cout << myID << ": i have file " << argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)] << endl; 

        // apply map
        if(myID < argc - 1){
            mapFile(argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)], countedWords);
        }
    }
    printMap( countedWords );
    //cout << "Mapsize is " << mapSize( countedWords ) << endl;
    string serialMap = serializeMap( countedWords );
		cout << serialMap << endl;
    //cout << serialMap << endl;
    // reduce
    // done

    MPI::Finalize();
    return EXIT_SUCCESS;
}
/* ----------  end of function main  ---------- */

