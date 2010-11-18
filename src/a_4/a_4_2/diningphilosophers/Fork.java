/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package diningphilosophers;

/**
 *
 * @author jan
 */

public class Fork {

    private boolean taken = false;

    public synchronized boolean take() {
        while(taken);
        taken = true;
        return true;
    }

    public synchronized void putDown(){
        this.taken = false;
    }

    public boolean isTaken(){
        return taken;
    }
}