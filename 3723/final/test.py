import re
from sys import argv

filein = open(argv[1])
filelines = filein.readlines()
last = filelines[-1]


for line in filelines:
      new = line.split('.')
      newre = re.split(',',line)
      print (new)
      print (newre)
