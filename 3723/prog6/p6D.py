#################################################################################################
# 				p5Driver.py written by Jonathan Jimenez
# Purpose:
#		This program reads an input file with commands per line and places each
#		of the values into dictionaries then prints them out.
# Command Parameters:
#    python3 p5Driver.py p5input.txt
#		The input file conatains commands. These commands specify FORMAT, VAR, and PRINT
#		which the program will execute.
#		@. FORMAT format=value....
#		@. VAR @varName=value
#		@. PRINT VARS/FORMAT
# Results:
# 		The output is written on a txt file: output.txt
#		prints the VAR dictionary, FORMAT dictionary, and error messages
# Returns:
#		N/A
##################################################################################################
from p6At import setVariable, setFormat
import re
import fileinput

#Dictionaries used for setting up variables, formats, and values
VAR = dict()
FORMAT = {"JUST": "LEFT", "BULLET":"o", "RM":"80","LM":"1","FLOW":"YES"}
filein = fileinput.input()
fl = open(filein)
#Iterating through every line in the file
for line in fileinput.input():
    
    matched = False
   
    #Regular expression that matches the lines and separates commands and rest
    lineRe = re.compile(r'@\.\s*(\w*)\s(.*)$')
    if lineRe.match(line):
          matched = True
          match = lineRe.search(line)
   
    if matched == True:
    #Checks to see if command matches "VAR". Calls setVariable if true
       if (match.group(1) == "VAR"): 
          setVariable(match.group(2),VAR)
	   
    #Checks to see if command matches FORMAT. Calls setFormat if true
       elif (match.group(1) == "FORMAT"):
          setFormat(match.group(2), FORMAT) 
	   
    #Checks to see if command matches PRINT. Prints the specified dictionary if true
       elif (match.group(1) == "PRINT"):
          if match.group(2) == "VARS":
            print(VAR)
          elif match.group(2) == "FORMAT":
            print(FORMAT)

for i in range(0,len(fl)):
    print (i)	  
