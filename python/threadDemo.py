import time
from downey import *

def thunkA():
    print "a1"
    print "a2"
    print "a3"
    
def thunkB():
    print "b1"
    print "b2"

def procC(x, y, z):
    print "c1", x, y, z
    print "c2", x, y, z

thunks = [thunkA, thunkB, procC]
    
for x in thunks:
    if x == procC:
        x(1, 2, 3)
    else:
        x()

for x in thunks:
    if x == procC:
        Thread(x, 1, 2, 3)
    else:
        Thread(x)

