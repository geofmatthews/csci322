// Matthews2015:  Added asynchronous readers and writers
//  Readers/Writers with parallel access
//
// Usage:
//         javac rw.parallel.java
//         java Main rounds
//

class RWbasic { // basic read or write; no exclusion
  protected int data = 0;  // the "database"
  protected void read() {
      System.out.println("read:  " + data);
  }
  protected void write() {
    data++;
    //System.out.println("wrote:  " + data);
  }
}

class Reader extends Thread {
  int rounds;
  RWbasic RW;
  public Reader(int rounds, RWbasic RW) {
    this.rounds = rounds;
    this.RW = RW;
  }
  public void run() {
    for (int i = 0; i<rounds; i++) {
	//RW.read();
    }
  }
}

class Writer extends Thread {
  int rounds;
  RWbasic RW;
  public Writer(int rounds, RWbasic RW) {
    this.rounds = rounds;
    this.RW = RW;
  }
  public void run() {
    for (int i = 0; i<rounds; i++) {
      RW.write();
    }
  }
}

class Main {  // driver program
    static int nThreads = 10;
    static RWbasic RW = new RWbasic();
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
	RW.read();
    }
}
