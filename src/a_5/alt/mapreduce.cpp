/*
 * =====================================================================================
 *
 *       Filename:  mapreduce.cpp
 *    Description:  handy functions for Mapping&Reducing some Textfiles
 *
 * =====================================================================================
 */

#include <algorithm>
#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <cctype>
#include <stdlib.h>
#include <string>
#include <cstdlib>
#include <map>
#include <utility>
#include "mapreduce.h"

using namespace std;

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  tokenize
 *  Description:  simple string tokenizer
 * =====================================================================================
 */
void tokenize(const string &str, vector<string> &tokens){

    const string delimiters = "1234567890 \"\n\t.,;:-+/?!()[]%#~'$*_^&§„";

    string::size_type tokenBegin = str.find_first_not_of(delimiters, 0);
    string::size_type tokenEnd = str.find_first_of(delimiters, tokenBegin);

    while (string::npos != tokenBegin || string::npos != tokenEnd){
        //add token to vector
				string test = toLower(str.substr(tokenBegin, tokenEnd - tokenBegin));

#ifndef NDEBUG
				/*  we only check for non-printable characters if we are in debugging mode. */
				for (string::iterator it = test.begin (); it != test.end (); it++) {
					if (!isprint ((*it))) {
						cerr << "Found non-printable character: " << (*it) << " = " << (int) (*it) << endl;
					}
				}
#endif

				tokens.push_back(test);
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
}		/* -----  end of function map  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  reduce
 *  Description:  
 * =====================================================================================
 */
map<string, int> reduce ( string &toReduce )
{
    map<string,int> bufMap;
    string delimiter = "\n";
    string::size_type tokenBegin = toReduce.find_first_not_of(delimiter, 0);
    string::size_type tokenEnd = toReduce.find_first_of(delimiter, tokenBegin);

    while (string::npos != tokenBegin || string::npos != tokenEnd){
        //get Key:Value Pair from toReduce
        string keyValuePair = toReduce.substr(tokenBegin, tokenEnd - tokenBegin);
	string::size_type pos = keyValuePair.find(":");
        string key = keyValuePair.substr(0,pos);
 	//printf("%d,",atoi(keyValuePair.substr(++pos).c_str())); 
  	int value = atoi(keyValuePair.substr(++pos).c_str());
	//build the reduced Map bye inserting the sepperated Key Value pairs
        pair<map<string, int>::iterator, bool> ret = bufMap.insert(pair<string, int>(key, value));
        if (!ret.second) 
            (*(ret.first)).second += ret.second;
        //and find new token
        tokenBegin = toReduce.find_first_not_of(delimiter, tokenEnd);
        tokenEnd = toReduce.find_first_of(delimiter, tokenBegin);
    }


    return bufMap;
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

map<int, string> mapMessages (map<int, string> &messageMap, map<string, int> &countedWords, int numPEs) {
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

void saveMapToFile(map<string, int> toSave, int id){
	string filename = "";

	char *buf = new char[NUMBEROFDIGITSINANINTEGER+2];
	sprintf (buf, "%d", id);
	// TODO unugliefy
	filename = "reduced-";
	filename += buf;
	filename += ".txt";

	ofstream file (filename.c_str ());
	if (!file.is_open()) {
		cerr << "Couldn't open " << filename << ". Aborting." << endl;
		std::abort ();
	}

	map<string,int>::iterator it;
	for (it = toSave.begin() ; it != toSave.end(); it++){
		file << (*it).first << ":" << (*it).second << endl;
	}
}


