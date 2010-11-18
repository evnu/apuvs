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
        int numberOfPhilosophers = 5;
        Fork[] forks = new Fork[numberOfPhilosophers];
        Philosopher[] philosophers =new Philosopher[numberOfPhilosophers];

        if(args.length > 1)
            numberOfPhilosophers = Integer.parseInt(args[0]);

        for(int i = 0; i < numberOfPhilosophers; i++){
            forks[i] = new Fork();
        }

        for(int i = 0; i < numberOfPhilosophers; i++){
            philosophers[i] = new Philosopher(names[i],
                    forks[i % numberOfPhilosophers],
                    forks[(i + 1) % numberOfPhilosophers]);
            philosophers[i].start();
            try {
                sleep(3000l);
            } catch (InterruptedException ex) {
                Logger.getLogger(Philosopher.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
}


