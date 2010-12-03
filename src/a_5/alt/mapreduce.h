/*
 * =====================================================================================
 *
 *       Filename:  mapreduce.h
 *    Description:  Tools for MapReduce in MPI
 *        Created:  02.12.2010 21:49:24
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

#define NUMBEROFDIGITSINANINTEGER 11
#define REDUCE 0
#define MARKER 1


void tokenize(const std::string&, std::vector<std::string>&);
void mapFile (char*, std::map<std::string,int>&);
std::map<std::string, int> reduce (std::string&);
void printMap (std::map<std::string, int> &toPrint);
int mapSize (std::map<std::string, int> toCount);
std::string serializeTuple (std::pair<const std::basic_string<char>, int> &serialize);
int wordToPE(const std::string &word, int numPEs);
char convertMe (char c);
std::string toLower (std::string str);
std::map<int, std::string> mapMessages (std::map<int, std::string> &messageMap, std::map<std::string, int> &countedWords, int numPEs);
void saveMapToFile(std::map<std::string, int> toSave, int id);

