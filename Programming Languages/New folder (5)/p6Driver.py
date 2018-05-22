#################################################################################################
# 				p6Driver.py written by Jonathan Jimenez
# Purpose:
#		This program reads an input file with commands per line and places each
#		of the values into dictionaries then prints them out.
# Command Parameters:
#    python3 p5Driver.py p5input.txt
#		The input file conatains commands. These commands specify FORMAT, VAR, and PRINT
#		which the program will execute.
#		@. FORMAT format=value....
#		@. VAR @varName=value
# Results:
# 		The output is written on a txt file: output.txt
#		prints formatted text according to the FORMAT and VAR commands
# Returns:
#		N/A
##################################################################################################
from p6At import setVariable, setFormat
from p6Format import varLine, formatLine
import re
from sys import argv

#following will open file and hold the last line in variable "last"
filein = open(argv[1])
filelines = filein.readlines()
last = filelines[-1]

#Dictionaries used for setting up variables, formats, and values
VAR = dict()
FORMAT = {"JUST": "LEFT", "BULLET":"o", "RM":"80","LM":"1","FLOW":"YES"}
#Iterating through every line in the file
for line in filelines:
    matched = False


    #Regular expression that matches the lines and separates commands and rest
    lineRe = re.compile(r'@\.\s*(\w*)\s(.*)$')
    if lineRe.match(line):
          matched = True
          match = lineRe.search(line)
    
    #Checks only for the lines with @. CMD
    if matched == True:
    #Checks to see if command matches "VAR". Calls setVariable if true
       if (match.group(1) == "VAR"):
          setVariable(match.group(2),VAR)

    #Checks to see if command matches FORMAT. Calls setFormat if true
       elif (match.group(1) == "FORMAT"):
          setFormat(match.group(2), FORMAT)
          formatLine(line,FORMAT,last)

    #Checks to see if command matches PRINT. Prints the specified dictionary if true
       elif (match.group(1) == "PRINT"):
          if match.group(2) == "VARS":
            print(VAR)
          elif match.group(2) == "FORMAT":
            print(FORMAT)
    
    #Checks for every other line.
    else:
        #used to print numbers for debugging (not sure if needed)
        numRe = re.compile(r'^\d+$|\s*\d+')
        if numRe.match(line):
            print(line, end="")
            continue
        #calls varLine to replace @varnames 
        outline = varLine(line, VAR)
        #new string only holds the lines with @varnames, otherwise is a string with None.
        #checks if strign is None, formatLine takes current iterating line instead
        if outline != None:
            formatLine(outline,FORMAT,last)
        elif outline == None or line is last:
            formatLine(line,FORMAT,last)
                                                                                         
