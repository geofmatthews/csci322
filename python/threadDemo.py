import time
from downey import *

def threadA():
    print "a1"
    print "a2"
    print "a3"
    
def threadB():
    print "b1"
    print "b2"

def threadC(x, y, z):
    print "c1", x, y, z
    print "c2", x, y, z

for thread in [threadA, threadB, threadC]:
    if thread == threadC:
        Thread(thread, 1, 2, 3)
    else:
        Thread(thread)

time.sleep(1)
