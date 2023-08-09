import folder1.mymodule as mymodule

print('Methods:' + str(dir(mymodule)))
mymodule.x()

from folder1.mymodule import *
x()
