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
 *       NOTE: Compile with -DNDEBUG to disable assert and debugging messages.
 *
 * =====================================================================================
 */
#include "mapreduce.h"
#include <mpi.h>

using namespace std;
/* 
 * ===  FUNCTION  ======================================================================
 *         Name:  main
 *  Description:  checks commandline-arguments and does all MPI stuff
 * =====================================================================================
 */

int main (int argc, char *argv[]) {
    if (argc < 2){
        cerr	<< "Not enough arguments\nPlease specify a list of files to be wordcounted" << endl;
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
		 * big, omit this step and wait for the reduce step. so there is no
         * limitation of PEs but it can happen that some PEs are idle
     */
    map<string, int> countedWords;
		if (myID < argc - 1){
            /*
             * this takes care of the distribution of the files; every PE gets a
             * base chunk of #files/#PEs; if there is a rest n the first n PEs
             * get their base chunk plus one; since we assume even distribution
             * of length of the files this is a good load balancing
             */
			for (int i = 0; i < (myID + 1 <= rest ? length + 1 : length); i++){
				// apply map
				mapFile(argv[(myID + 1 <= rest ? myID * length + 1 + myID + i: myID * length + 1 + rest + i)], countedWords);
			}
    }

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
		MPI_Request request;
		for (map<int,string>::iterator it = messageMap.begin (); it != messageMap.end (); it ++) {
			string &message = (*it).second;
			// only send if this if the receiver != sender
			if ((*it).first != myID) 
				MPI_Isend ((void*) message.c_str (), message.size () + 1, MPI_CHAR, (*it).first, 0, MPI_COMM_WORLD, &request);
		}
		
		/* Wait for messages from all PEs */ 
		
#ifndef NDEBUG
		bool *doneWithPE = new bool[numPEs];
		for (int i = 0; i < numPEs; i++) {
			doneWithPE[i] = false;
		}
		doneWithPE[myID] = true;
#endif

		int numberOfFinishedMessages = numPEs - 1;
		
		while (numberOfFinishedMessages) {
			MPI_Status status;
			int msglen;

			// check for new messages
			MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
			MPI_Get_count (&status, MPI_CHAR, &msglen);

#ifndef NDEBUG
			assert (status.MPI_SOURCE >= 0 && status.MPI_SOURCE < numPEs); // sanity check
			assert (!doneWithPE[status.MPI_SOURCE]);
			doneWithPE[status.MPI_SOURCE] = true;
#endif

			char *buf;

			// As we don't know how big the messages will be, there must be some error handling...
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
			numberOfFinishedMessages--;
		}

#ifndef NDEBUG
delete[] doneWithPE;
#endif

/* reduce */
map<string, int> final = reduce(messageMap[myID]);

/* final steps - save the result */
saveMapToFile (final, myID);

/* We are done here - clean up the mess */ 
MPI::Finalize();
return EXIT_SUCCESS;
}
/* ----------  end of function main  ---------- */

