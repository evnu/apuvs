/*
 * =====================================================================================
 *
 *       Filename:  mapreduce.cpp
 *    Description:  handy functions for Mapping&Reducing some Textfiles
 *
 * =====================================================================================
 */

#include "mapreduce.h"

// NUMBEROFDIGITSINANINTEGER is a rough estimate on the length of an integer. An int is
// probably shorter, but as we calculate the needed size for a given string anyways, that
// doesn't matter to much.

using namespace std;

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  tokenize
 *  Description:  simple string tokenizer
 * =====================================================================================
 */
void tokenize(const string &str, vector<string> &tokens){

    //all non-charracters are token delimiters
    const string delimiters = "1234567890 \"\n\t.,;:-+/?!()[]„";

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
 *  Description:  opens file and processes it line by line; emits a map of type
 *  <string,int> maps where the integer is the amount of occurences of the key in the parameter
 *  file; we don't emit a multimap of <word, 1> pairs (some would consider this a purer 
 *  map-reduce implementation) in order to minimize data that needs to be send via MPI
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
             
						// construct the actual map
            for (vector<string>::iterator i = token.begin(); i != token.end(); i++){
                ret = outputMap.insert(pair<string, int>(*i, 1));
                if (!ret.second){
                    (*(ret.first)).second++;
                }
            }
        }
    }
    else cerr << "Couldn't open File: " << fileName << endl;   
}		/* -----  end of function map  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  reduce
 *  Description:  reduce() deserializes the parameter string, adds all
 *  occurences of the same word and emits a map<string, int> with unique words
 *  and their number of occurence in every file that was in the commandline-arguments of
 *  the programmcall
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
 *  Description:  calculate size of map including \n for each key and value; is
 *  used to calculate the size 
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

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  serializeTuple
 *  Description:  takes a pair<string, int> and returns it as a serialized
 *  string of the form "string:int\n"
 * =====================================================================================
 */
string serializeTuple (pair<const basic_string<char>, int> &serialize) {
	string str (serialize.first);
	// we add 2 to NUMBEROFDIGITSINANINTEGER to make sure that the string is delimited with
	// \0 
	char *buf = new char[NUMBEROFDIGITSINANINTEGER+2];
	sprintf (buf, ":%d\n", serialize.second);
	str += buf;
	delete[] buf;
	return str;
}		/* -----  end of function serializeTuple  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  wordToPE
 *  Description:  takes a word an decides which PE gets it to reduce
 *  it the "number" of the ascii character (first character -'a') and returns
 *  this number modulo the number of PEs.
 *  this can result in a bad load balancing since we do not consider the
 *  distribution of characters in the language present.
 * =====================================================================================
 */
int wordToPE(const string &word, int numPEs){
	char first = *(word.c_str());
	return (abs(first - 'a')) % numPEs;
}		/* -----  end of function wordToPE  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  convertME
 *  Description:  needed for toLower()
 * =====================================================================================
 */
char convertMe (char c) {
	return tolower (c);
}		/* -----  end of function convertME  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  toLower
 *  Description:  returns the parameter string with only lower characters
 * =====================================================================================
 */
string toLower (string str) {
	transform (str.begin (), str.end (), str.begin (), convertMe);
	return str;
}		/* -----  end of function toLower  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  mapMessages
 *  Description:  takes a map<string, int> and the number of available PEs and
 *  emits a map<int, string> where the key is the PE that gets the value; the
 *  value is a string which is a serialized map of pairs <word, occurence> that
 *  is meant to be reduced by one PE
 * =====================================================================================
 */
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
}		/* -----  end of function mapMessages  ----- */

/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  saveMapToFile
 *  Description:  saves a map to file "reduced-#PE.txt" with the formating
 *  word:occurence
 * =====================================================================================
 */
void saveMapToFile(map<string, int> toSave, int id){
	stringstream filename;
	filename << "reduced-" << id << ".txt";
	
	ofstream file (filename.str().c_str ());
	if (!file.is_open()) {
		cerr << "Couldn't open " << filename << ". Aborting." << endl;
		std::abort ();
	}

	for (map<string,int>::iterator it = toSave.begin() ; it != toSave.end(); it++){
		file << (*it).first << ":" << (*it).second << endl;
	}
}


