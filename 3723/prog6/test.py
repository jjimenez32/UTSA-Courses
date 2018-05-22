from sys import argv
import textwrap

filer = open(argv[1])
lines = list()
for line in filer:
   lines.append(line)
newline = "\n".join(lines)
newline = textwrap.fill(newline,70)
print (newline.rjust(90))
nlines = newline.split("\n")
for i in range(0,len(nlines)):
     line = lines[i].rjust(90,"*")
     print (nlines[i].rjust(90,"*")) 
