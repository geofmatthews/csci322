from downey import *

def func1(x,y,z):
    print "func1",x,y,z
    sem.signal()
    
def func2(x,y,z):
    sem.wait()
    print "func2",x,y,z
    
sem = Semaphore(0)

#threads = [Thread(f, 1, 2, 3) for f in (func1, func2)]

Thread(func2, 1, 2, 3)
time.sleep(1)
Thread(func1, 4, 5, 6)
time.sleep(1)
