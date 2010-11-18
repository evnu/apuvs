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

    public void take() {
        while(taken);
        changeStatus(true);
    }

    public void putDown(){
        while(!taken);
        changeStatus(false);
    }

    public boolean isTaken(){
        return taken;
    }

    private synchronized void changeStatus(boolean newStatus){
        taken = newStatus;
    }
}