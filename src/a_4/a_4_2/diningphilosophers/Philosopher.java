/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package diningphilosophers;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author jan
 */
public class Philosopher extends Thread{

    private String me;
    private Fork left, right;

    public Philosopher(String name, Fork left, Fork right) {
        this.me = name;
        this.left = left;
        this.right = right;
    }

    @Override
    public void run() {

        while(true){
//----------try to get left fork------------------------------------------------
            left.take();
            System.out.println(me + " got his left fork...WOOHOO");

            /*try {
                sleep(2005l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE,
                        null, ex);
            }*/
//----------try to get right fork-----------------------------------------------
            /*if (right.isTaken()) {
                left.putDown();
                System.out.println(me + " puts his left fork down....right fork is used");
                try {
                    sleep(2000l);
                } catch (InterruptedException ex) {
                    Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE,
                            null, ex);
                }
                continue;
            }*/
                /*
                 * to prevent deadlock situation put down left fork
                 * in case right fork is taken
                 * to do this simply uncomment if-block above
                 * this violates the Hold-and-Wait condition of the
                 * Coffman conditions
                 */
            right.take();
            System.out.println(me + " got his right fork...WOOHOO");
                    

//----------if having both forks eat for a while and put down forks-------------
            System.out.println(me + " is eating...nomnomnomnomnomnomnomnomnomnomn");
            /*try {
                sleep(5000l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE,
                        null, ex);
            }*/
            left.putDown();
            right.putDown();
            System.out.println(me + " puts down his dirty forks");
        }
    }
}
