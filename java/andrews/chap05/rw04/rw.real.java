//  Readers/Writers with concurrent read or exclusive write
//
// Usage:
//         javac rw.real.java
//         java Main rounds

class RWbasic {         // basic read or write
    protected int data = 0;  // the "database"
    protected void read() {
	System.out.println("read:  " + data);
    }
    protected void write() {
	data++;
	System.out.println("wrote:  " + data);
    }
}

class ReadersWriters extends RWbasic {  // Readers/Writers
    int nr = 0;
    private synchronized void startRead() {
	nr++;
    }
    private synchronized void endRead() {
	nr--;
	if (nr==0) notify();  // awaken waiting Writers
    }
    public void read() {
	startRead();
	//System.out.println("read:  " + data);
	endRead();
    }
    public void cat() {
	System.out.println("read:  " + data);
    }
    public synchronized void write() {
	while (nr>0)
	    try { wait(); } 
	    catch (InterruptedException ex) {return;}
	data++;
	//System.out.println("wrote:  " + data);
	notify();    // awaken another waiting Writer
    }
}

class Reader extends Thread {
    int rounds;
    ReadersWriters RW;
    public Reader(int rounds, ReadersWriters RW) {
	this.rounds = rounds;
	this.RW = RW;
    }
    public void run() {
	for (int i = 0; i<rounds; i++) {
	    RW.read();
	}
    }
}

class Writer extends Thread {
    int rounds;
    ReadersWriters RW;
    public Writer(int rounds, ReadersWriters RW) {
	this.rounds = rounds;
	this.RW = RW;
    }
    public void run() {
	for (int i = 0; i<rounds; i++) {
	    RW.write();
	}
    }
}

class Main {  
    static int nThreads = 10;
    static ReadersWriters RW = new ReadersWriters();
    static Reader[] RArray = new Reader[nThreads];
    static Writer[] WArray = new Writer[nThreads];
    public static void main(String[] arg) throws InterruptedException {
	int rounds = Integer.parseInt(arg[0],10);
	for (int i = 0; i < nThreads; i++) {
	    RArray[i] = new Reader(rounds, RW);
	    RArray[i].start();
	    WArray[i] = new Writer(rounds, RW);
	    WArray[i].start();
	}
	for (int i = 0; i < nThreads; i++) {
	    RArray[i].join();
	    WArray[i].join();
	}
	RW.cat();
    }
}
