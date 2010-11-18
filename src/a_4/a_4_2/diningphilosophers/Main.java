/*
 * 
 */

package diningphilosophers;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author jan
 */
public class Main extends Thread{

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        String[] names = {"Machiaveli", "Russel", "Sokrates",
                            "Kant", "Leibnitz"};

        int numberOfPhilosophers = names.length;

        Fork[] forks = new Fork[numberOfPhilosophers];
        Philosopher[] philosophers = new Philosopher[numberOfPhilosophers];

        for(int i = 0; i < numberOfPhilosophers; i++){
            forks[i] = new Fork();
        }

        for(int i = 0; i < numberOfPhilosophers; i++){
            philosophers[i] = new Philosopher(names[i],
                    forks[(i + 1) % (numberOfPhilosophers)],
                    forks[i % (numberOfPhilosophers)]);
            philosophers[i].start();


            /* The following code is needed if we decide to 
						 * use sleep sometime later
						 *
						 * try {
                sleep(2l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE, null, ex);
            }*/
        }
    }
}


