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

    const string delimiters = " \n\t.,;:-+/?!()[]";

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
 *         Name:  main
 *  Description:  checks cl-arguments and does all MPI stuff
 * =====================================================================================
 */
    int
main ( int argc, char *argv[] )
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
        // open files and read content
        int fileLength = 0;
        char *buffer;

        ifstream file(argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)]);
        if(file.is_open()){
            //find out length of file
            file.seekg(0, ios::end);
            fileLength = file.tellg();
            file.seekg (0, ios::beg);
            //create apropriate buffer
            buffer = new char[fileLength];

            //read the whole file as a block
            file.read(buffer, fileLength);
            file.close();
        }

        //tokenize
        vector<string> tokens;
        Tokenize(buffer, tokens);
        for(vector<string>::iterator i = tokens.begin(); i != tokens.end(); i++){
            cout << *i << endl;
        }

        // apply map
        // reduce
        // done
    }

    MPI::Finalize();
    return EXIT_SUCCESS;
}
/* ----------  end of function main  ---------- */

