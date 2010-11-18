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
    private boolean hasLeft, hasRight;

    public Philosopher(String name, Fork left, Fork right) {
        this.me = name;
        this.left = left;
        this.right = right;
    }

    @Override
    public void run() {

        while(true){
            System.out.println(me + "is trying to take left fork.");
            if (left.take()){
                this.hasLeft = true;
                System.out.println("YAY..." + me + " got the left fork");
            }
            try {
                sleep(2000l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE, null, ex);
            }
            System.out.println(me + "is trying to take right fork");
            if (right.take()){
                this.hasRight = true;
                System.out.println("YAY..." + me + " got the right fork");
            }
            try {
                sleep(2000l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE, null, ex);
            }
            if (hasLeft && hasRight){
                System.out.println(me + " is eating...nomnomnom");
                try {
                    sleep(5000l);
                } catch (InterruptedException ex) {
                    Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE, null, ex);
                }
                System.out.println(me + " puts down froks");
                left.putDown();
                right.putDown();
                hasLeft = hasRight = false;
            }
            try {
                sleep(2000l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
}
