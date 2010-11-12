/**
 *
 * @author jan
 */

import java.util.ArrayList;

public class apuvs3 {


    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {

        ArrayList<Thread> list = new ArrayList<Thread>();
        if(args.length == 0 || !(args[0].equals("a") || args[0].equals("b"))){
            System.out.println("Usage: apuvs3 a|b");
            System.out.println("a uses sleeping threads, b uses threads doing calculation");
            System.out.println("use STRG+C to kill the program...otherwise it will continue to create threads");
            System.exit(-1);
        }

        while(true){
            Thread thread = null;
            if(args[0].equals("a")){
                thread = new DoSleeping();
            }
            if(args[0].equals("b")){
                thread = new DoALittle();
            }
            list.add(thread);
            System.out.println("Starting thread #" + Integer.toString(list.size()));
            thread.start();
        }
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
