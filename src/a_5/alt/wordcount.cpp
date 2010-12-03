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
#include "mapreduce.h"
#include <mpi.h>

using namespace std;
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

		// initialize message map with empty messages
		map<int, string> messageMap;
		for (int i = 0; i < numPEs; i++) 
			messageMap[i] = "";

	  mapMessages (messageMap, countedWords, numPEs);

		/* Send the actual messages to the PEs */ 
		for (map<int,string>::iterator it = messageMap.begin (); it != messageMap.end (); it ++) {
			string &message = (*it).second;
			// only send if this if the receiver != sender
			if ((*it).first != myID) 
				MPI::COMM_WORLD.Send((void*) message.c_str (), message.size () + 1, MPI::CHAR, (*it).first, REDUCE);
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
			int msglen;

			MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
			MPI_Get_count (&status, MPI_CHAR, &msglen);

			assert (status.MPI_SOURCE >= 0 && status.MPI_SOURCE < numPEs); // sanity check
			assert (!doneWithPE[status.MPI_SOURCE]);
			doneWithPE[status.MPI_SOURCE] = true;

			// wether we got a message with a certain length or not, we recognize this
			// communication as finished
			if (!msglen) {
				// throw message away
				MPI_Recv ((void*)0, 0, MPI_CHAR, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, &status);
			} else {
				int msglen;
				MPI_Get_count (&status, MPI_CHAR, &msglen);
				
				char *buf;
				
				// As we don't know how big the messages will be, there must be some error handling. 
				try {
					buf = new char[msglen];
				} catch (std::bad_alloc) {
					cerr << "Fatal error: Couldn't allocate enough memory for the message. Aborting." << endl;
					std::abort ();
				}

				// receive message
				MPI_Recv (buf, msglen, MPI_CHAR, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, &status);

				// add the message to the PEs workload
				messageMap[myID] += buf;

				delete[] buf;
			}
			numberOfFinishedMessages--;
		}
		
		delete[] doneWithPE;

		/* reduce */
		map<string, int> final = reduce(messageMap[myID]);

		saveMapToFile (final, myID);

		// save result

		/*We are done here - clean up the mess*/ 
    MPI::Finalize();
    return EXIT_SUCCESS;
}
/* ----------  end of function main  ---------- */

