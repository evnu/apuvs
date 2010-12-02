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

#include <algorithm>
#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <cctype>
#include <mpi.h>
#include <stdlib.h>
#include <string>
#include <cstdlib>

#define NUMBEROFDIGITSINANINTEGER 11
#define REDUCE 0
#define MARKER 1

using namespace std;

// prototypes
string toLower (string);

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  tokenize
 *  Description:  simple string tokenizer
 * =====================================================================================
 */
void tokenize(const string &str, vector<string> &tokens){

    const string delimiters = "1234567890 \"\n\t.,;:-+/?!()[]„";

    string::size_type tokenBegin = str.find_first_not_of(delimiters, 0);
    string::size_type tokenEnd = str.find_first_of(delimiters, tokenBegin);

    while (string::npos != tokenBegin || string::npos != tokenEnd){
        //add token to vector
        tokens.push_back(toLower(str.substr(tokenBegin, tokenEnd - tokenBegin)));
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
void mapFile (char* fileName, map<string,int> &outputMap)
{
    pair<map<string,int>::iterator,bool> ret;
    string line;
    ifstream file(fileName);
    if (file.is_open()){
        while (getline(file, line)){
            vector<string> token;
            tokenize(line, token);
            
            for (vector<string>::iterator i = token.begin(); i != token.end(); i++){
                ret = outputMap.insert(pair<string, int>(*i, 1));
                if (!ret.second){
                    (*(ret.first)).second++;
                }
            }
        }
    }
    else cout << "Couldn't open File: " << fileName << endl;   
    
    return ;
}		/* -----  end of function map  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  reduce
 *  Description:  
 * =====================================================================================
 */
void reduce ( string &toReduce, map<string, int> &reduced )
{
    string delimiters = "\n";
    string::size_type begin = toReduce.find_first_not_of(delimiters, 0);
    string::size_type end = toReduce.find_first_of(delimiters, begin);

    while(string::npos != begin || string::npos != end){
    //what's supposed to be here?...
    }

    return ;
}		/* -----  end of function reduce  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  printMap
 *  Description:  just for debuging
 * =====================================================================================
 */
void printMap (map<string, int> &toPrint)
{
    map<string,int>::iterator it;
    for (it=toPrint.begin() ; it != toPrint.end(); it++)
            cout << (*it).first << " => " << (*it).second << endl;
    return ;
}		/* -----  end of function printMap  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  mapSize
 *  Description:  calculate size of map including \n for each key and value
 * =====================================================================================
 */
int mapSize (map<string, int> toCount)
{
    map<string,int>::iterator it;
    int size = 0;
    for (it=toCount.begin() ; it != toCount.end(); it++){
        size += (*it).first.length() * sizeof(char) + 1;
        size += sizeof((*it).second) + sizeof(char);
    }
    return size;
}		/* -----  end of function mapSize  ----- */

string serializeTuple (pair<const basic_string<char>, int> &serialize) {
	string str (serialize.first);
	char *buf = new char[NUMBEROFDIGITSINANINTEGER+2];
	sprintf (buf, ":%d\n", serialize.second);
	str += buf;
	delete[] buf;
	return str;
}

int wordToPE(const string &word, int numPEs){
	char first = *(word.c_str());
	return (abs(first - 'a')) % numPEs;
}

char convertMe (char c) {
	return tolower (c);
}

string toLower (string str) {
	transform (str.begin (), str.end (), str.begin (), convertMe);
	return str;
}

map<int, string> mapMessages (map<string, int> &countedWords, int numPEs) {
		map<int, string> messageMap; 
		for (map<string, int>::iterator it = countedWords.begin (); it != countedWords.end (); it++) {
			// determine which pe needs this message
			int receiver = wordToPE ((*it).first, numPEs);

			// serialize a single pair
			string serializedMessage = serializeTuple (*it);

			// try to insert new message into message mapper
			pair<map<int,string>::iterator,bool> ret = messageMap.insert(pair<int,string> (receiver, serializedMessage));

			// if the receiver already existed in the map (and therefore insert failed), just append the message
			if (!ret.second) 
				(*(ret.first)).second += serializedMessage;
		}

		return messageMap;
}
/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  main
 *  Description:  checks cl-arguments and does all MPI stuff
 * =====================================================================================
 */

int main (int argc, char *argv[]) {
    if (argc < 2){
        cout	<< "Not enough arguments\nPlease specify a list of files to be wordcounted" << endl;
        exit(-2);
    }

    //initialize MPI stuff
    MPI::Init(argc, argv);

    int numPEs = MPI::COMM_WORLD.Get_size();
    int myID = MPI::COMM_WORLD.Get_rank();

    int length, rest;

    length = (argc - 1) / numPEs; //base chunk for every PE
    rest = (argc - 1) % numPEs; //rest of files to spread around

    /*
		 * map
		 *
		 * only apply map if there are enough files to look at. if the rank of the pe is to
		 * big, omit this step and wait for the reduce step.
     */
    map<string, int> countedWords;
		if (myID < argc - 1){
			for (int i = 0; i < (myID + 1 <= rest ? length + 1 : length); i++){
				// apply map
				mapFile(argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)], countedWords);
			}
    }
    //printMap(countedWords);

		/* We successfully fullfiled the map step. Now we have to find out, to which PEs we
		 * have to send a message containing (key,value) pairs. */ 
		
		// each pe receives 1-2 messages from the current pe. the first message (if send) contains the serialized maps. 
		// the second is a marker that we don't want to send any more messages
		map<int, string> messageMap = mapMessages (countedWords, numPEs);

		//for (map<int,string>::iterator it = messageMap.begin (); it != messageMap.end (); it ++) {
			//cout << myID << " - PE: " << (*it).first << " gets " << (*it).second << endl;
		//}

		/* Send the actual messages to the PEs */ 
		for (map<int,string>::iterator it = messageMapper.begin (); it != messageMapper.end (); it ++) {
			string &message = (*it).second;
			MPI::COMM_WORLD.Send((void*) message.c_str (), message.size (), MPI::CHAR, (*it).first, REDUCE);
			cout << myID << " send " << (*it).first << endl;
		}

		/* Send marker to each PE, indicating that we are done with it. */ 
		for (int pe = 0; pe < numPEs; pe++) {
			if (pe == myID) 
				continue;
			MPI::COMM_WORLD.Send((void*) 0, 0, MPI::CHAR, pe, MARKER);
		}
		
		/* Wait for messages from all PEs */ 
		bool *doneWithPE = new bool[numPEs];
		for (int i = 0; i < numPEs; i++) {
			doneWithPE[i] = false;
		}
		doneWithPE[myID] = true;

		int numberOfFinishedMessages = numPEs - 1;
		
		// TODO check if condition is right...
		string myMessages;
		while (numberOfFinishedMessages) {
			MPI_Status status;
			MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

			int msglen;
			MPI_Get_count (&status, MPI_CHAR, &msglen);
			// we recognize a message of length zero as the marker.
			if (status.MPI_TAG == MARKER) {
				assert (status.MPI_SOURCE >= 0 && status.MPI_SOURCE < numPEs); // sanity check
				assert (!doneWithPE[status.MPI_SOURCE]);
				doneWithPE[status.MPI_SOURCE] = true;
				
				// throw message away
				MPI_Recv ((void*)0, 0, MPI_CHAR, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, &status);
				numberOfFinishedMessages--;
			} else {
				int msglen;
				MPI_Get_count (&status, MPI_CHAR, &msglen);
				
				// receive message
				char *buf;
				
				// As we don't know how big the messages will be, there must be some error handling. 
				try {
					buf = new char[msglen];
				} catch (std::bad_alloc) {
					cerr << "Fatal error: Couldn't allocate enough memory for the message. Aborting." << endl;
					std::abort ();
				}
				MPI_Recv (buf, msglen, MPI_CHAR, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, &status);

				// add the message to the PEs workload
				messageMapper[myID] += buf;
				delete[] buf;
			}
		}
		
		delete[] doneWithPE;

		cout << myID << ": I received the following workload: " << messageMapper[myID] << endl;

		// receive messages

		/* reduce */

		/*We are done here - clean up the mess*/ 
    MPI::Finalize();
    return EXIT_SUCCESS;
}
/* ----------  end of function main  ---------- */

