//blablub to be added
import java.lang.Thread;
import java.util.*;
import java.io.*;

class JavaThreadTime extends Thread
{
 
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
  {  
	int max_thr=500;
 	JavaThreadTime[] jttarr = new JavaThreadTime[max_thr];
	Date begin = new Date();
	for(int i=0;i<max_thr;i++){
	jttarr[i] = new JavaThreadTime();
	jttarr[i].start();
	}
	float time = (System.currentTimeMillis()-begin.getTime());
	//System.out.print(time/max_thr);
	System.out.format("%d Processes created with %fs for each Process\n",max_thr,time/max_thr);
  }
}

