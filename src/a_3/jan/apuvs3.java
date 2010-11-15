/**
 *
 * @author jan
 */

import java.util.ArrayList;
import java.util.Date;

public class apuvs3 {


    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        if(args.length == 0 || !(args[0].equals("a") || args[0].equals("b")
                || args[0].equals("time"))){
            System.out.println("Usage: apuvs3 a #threads | b #threads | time #threads");
            System.out.println("a uses sleeping threads, b uses threads doing calculation, time times thread creation time");
            System.out.println("use STRG+C to kill the program...otherwise it will continue to create threads");
            System.exit(-1);
        }

        if(args[0].equals("a"))
            a(Integer.parseInt(args[1]));
        if(args[0].equals("b"))
            b(Integer.parseInt(args[1]));
        if(args[0].equals("time"))
            time(Integer.parseInt(args[1]));
        return;
    }

    private static void a(int i){
        ArrayList<Thread> list = new ArrayList<Thread>();
        while (i > list.size()) {
            Thread thread = new DoSleeping();
            thread.setDaemon(true);
            list.add(thread);
            System.out.println("Starting thread #" + Integer.toString(list.size()));
            thread.start();
        }
        return;
    }

    private static void b(int i) {
        ArrayList<Thread> list = new ArrayList<Thread>();
        while (i > list.size()) {
            Thread thread = new DoALittle();
            thread.setDaemon(true);
            list.add(thread);
            System.out.println("Starting thread #" + Integer.toString(list.size()));
            thread.start();
        }
        return;
    }

    private static void time(int numberOfThreads) {
        ArrayList<Thread> list = new ArrayList<Thread>();

        Date begin = new Date();
        for(int i = 0; i <= numberOfThreads; i++){
            list.add(new Thread());
        }
        Date end = new Date();
        Long time = (end.getTime() - begin.getTime());
        System.out.println(numberOfThreads + " "
                + Double.toString((double)time/(double)numberOfThreads) + " " + time);
        return;
    }
}

class DoALittle extends Thread {

    boolean run = true;
    int i = 0;

    @Override
    public void run() {
        try{
            while (run)
                i += 1;
        } catch(Exception e){
            e.printStackTrace();
        }
    }

    @Override
    public void finalize() throws Throwable {
        try {
            run = false;
        } catch (Exception e) {
        } finally {

            super.finalize();
            //more code can be written here as per need of application

        }
    }
}

class DoSleeping extends Thread {

    boolean run = true;

    @Override
    public void run() {
        try {
            while (run)
                sleep(1000l);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void finalize() throws Throwable {
        try {
            run = false;
        } catch (Exception e) {
        } finally {

            super.finalize();
            //more code can be written here as per need of application

        }
    }
}
