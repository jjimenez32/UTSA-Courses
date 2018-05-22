from sys import argv
import re
array = list()
files = open(argv[1])
lines = files.readlines()
lister = list()
for line in lines:
    array.append(line)
newline = "".join(array)
for line in newline:
    line.rstrip()
lim = 55
#re.sub("(.{64})", "\\1\n", s, 0, re.DOTALL)
for s in newline.split("\n"):
    if s == "":
        print()
        lister.append(s)
    w=0 
    l = []
    for d in s.split():
        if w + len(d) + 1 <= lim:
            l.append(d)
            w += len(d) + 1 
        else:
            print (" ".join(l))
            lister.append("".join(l))
            l = [d] 
            w = len(d)
    if (len(l)): 
        print (" ".join(l))
        lister.append("".join(l))

print (lister)
