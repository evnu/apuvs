/*Create max_thr Threads and let each one sleep... */
import java.lang.Thread;
import java.util.*;
import java.io.*;

class JavaThreadTime extends Thread
{
  //run Forrest r... wait a Second ... Sleep Forrest Sleep
  public void run(){
      try
      { 
        sleep(50);
      }
 
      catch(InterruptedException ie)
      {
       System.out.println("InterruptedException caught...");
      }
     
  }
}
 
 
public class Max_Jthr
{
  public static void main(String[] args)
  {  	/*max_thr number of Threads to create*/
	int max_thr=500;
 	JavaThreadTime[] jttarr = new JavaThreadTime[max_thr];	
	Date begin = new Date();				//save start-time
	for(int i=0;i<max_thr;i++){				//start Thread Creation
	jttarr[i] = new JavaThreadTime();			//Create new JavaThreadTime-Object
	jttarr[i].start();					//start the created Object
	}
	float time = (System.currentTimeMillis()-begin.getTime()); //get end-time and calc the delta
	System.out.format("%d Processes created with %fs for each Process\n",max_thr,time/max_thr); //print Seconds/Thread
  }
}

