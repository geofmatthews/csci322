from downey import *

fred = Semaphore()
fred.signal()
fred.wait()
print "Hello"
