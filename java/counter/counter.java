// counter.java
// Geoffrey Matthews 2015
// Demo of synchronized and unsynchronized methods

class Counter {
    private int c = 0;
    public void increment() {
	c++;
    }
    public int value() {
	return c;
    }
}

// Using synchronized methods turns the class into a monitor
class SynchronizedCounter {
    private int c = 0;
    public synchronized void increment() {
	c++;
    }
    public synchronized int value() {
	return c;
    }
}

class Incrementer extends Thread {
    int rounds;
    Counter C;
    public Incrementer(int rounds, Counter C) {
	this.rounds = rounds;
	this.C = C;
    }
    public void run() {
	for (int i = 0; i < rounds; i++) {
	    C.increment();
	}
    }
}

class SynchronizedIncrementer extends Thread {
    int rounds;
    SynchronizedCounter SC;
    public SynchronizedIncrementer(int rounds, SynchronizedCounter SC) {
	this.rounds = rounds;
	this.SC = SC;
    }
    public void run() {
	for (int i = 0; i < rounds; i++) {
	    SC.increment();
	}
    }
}

class Main {
    // The databases
    static Counter C = new Counter();
    static SynchronizedCounter SC = new SynchronizedCounter();

    // Arrays for the threads
    static Incrementer[] IArray = new Incrementer[10];
    static SynchronizedIncrementer[] SIArray = new SynchronizedIncrementer[10];

    public static void main(String[] arg) {
	int rounds = Integer.parseInt(arg[0],10);
	// Start unsynchronized threads
        for (int i = 0; i < 10; i++) {
	    IArray[i] = new Incrementer(rounds, C);
	    IArray[i].start();
	}
	for (int i = 0; i < 10; i++) {
	    try{
		IArray[i].join();
	    } catch (InterruptedException ex) {
		Thread.currentThread().interrupt();
	    }	    
	}
	System.out.println("Final unsynchronized value:  " + C.value());
	// Start synchronized threads
        for (int i = 0; i < 10; i++) {
	    SIArray[i] = new SynchronizedIncrementer(rounds, SC);
	    SIArray[i].start();
	}
	for (int i = 0; i < 10; i++) {
	    try{
		SIArray[i].join();
	    } catch (InterruptedException ex) {
		Thread.currentThread().interrupt();
	    }
	}
	System.out.println("Final synchronized value:  " + SC.value());
    }
}    

// Example runs:
// $ java Main 10
// Final unsynchronized value:  94
// Final synchronized value:  100
// $ java Main 100
// Final unsynchronized value:  813
// Final synchronized value:  1000
// $ java Main 1000
// Final unsynchronized value:  4981
// Final synchronized value:  10000
