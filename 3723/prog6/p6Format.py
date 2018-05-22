import re

#list that holds flow = yes lines
lines = list()
#list that holds flow = no lines
nlines = list()
#used to try and identify the current line as a line with @. CMD
oldFormat = {"JUST": "LEFT", "BULLET":"o", "RM":"80","LM":"1","FLOW":"YES"}


##################### varLine ####################################################################
# varLine (string line, dictionary varDictionary)
# Purpose:
#             Checks if line has a @ variable name and replaces it with the VAR dictionary key
# Parameters:
#  I    string	                    String that holds the current line of the file
#  I    dicionary                   Dictionary that holds every varName : value
#  O   	N/A
# Returns:
#       out/line                    String with replaced variable names
# Notes:
#       I used 2 regex to match a line with either only @varnames or lines that included
#       them. 
##################################################################################################
def varLine(line, varDictionary):
     #regex that check for @varnames in line
     lineRe = re.compile(r'@(\w*,?)')
     lineRe2 = re.compile(r'^.*$')
     matched = False
     #checks for a line that starts with @varname then replaces it accordingly
     if lineRe.match(line):
        matched = True
        match = lineRe.findall(line)
        for key in varDictionary:
           for i in range(0,len(match)):
              if key in match[i]:
                  match[i] = varDictionary[key]
        out =' '.join(match)
        return out

     #if not the regex above, checks for @varname in entire line and replaces them accordingly
     elif matched == False:
        if lineRe2.match(line):
          if '@' in line:
            atRe = re.compile(r'@(\w*)')
            matching = atRe.findall(line)
            for key in varDictionary:
              for i in range(0, len(matching)):
                if key in matching[i]:
                  line = re.sub(matching[i],varDictionary[key],line)
            line = line.replace("@", "")
            return line
################ setFormat ####################################################################
# formatLine (string line, dictionary formatDictionary, boolean last)
# Purpose:
#       Checks for current formatDictionary values to tell how the current 
#        line should be formatted
# Parameters:
#  I    string	            String that holds the current line in the file
#  I    dicionary           Dictionary that holds every format : value
#  I    boolean             Boolean passed in that signifies whether the current line is the 
#                           last line of the file
#  O   	N/A
# Returns:
#		N/A
# Notes: 
#       To check for the beginning of paragraphs, this checks whether the current line is 
#        an empty line or the last line. If not it will append the current line into a list. 
#       Once an empty line or the lastline is found, it joins all elements in list and sends it to 
#       the formatPrinter for further formatting then printing.
##################################################################################################
def formatLine(line, formatDictionary,last):
    
    #typecasting the formatDictionary LM and RM to ints to use 
    left = int(formatDictionary["LM"])
    right = int(formatDictionary["RM"])
    
    #these hold the previous command's variable for LM and RM
    oldLeft = int(oldFormat["LM"])
    oldRight = int(oldFormat["RM"])
    
    #variable to see if command is flow or not
    flow = True
    
    #setting up the use of the global variable for holding the previous line's formatDictionary
    global oldFormat
   
    #checks to see if the current line is a @. FORMAT command and strips the \n and sets it to empty string
    if "@. FORMAT" in line:
         line = ""
         line.rstrip("\n")
   
    #codes that represent the current "JUST" key that are sent to the printer
    if formatDictionary["JUST"] == "LEFT":
            justCode = 0
    elif formatDictionary["JUST"] == "BULLET":
            justCode = 1
    elif formatDictionary["JUST"] == "RIGHT":
            justCode = 2
    elif formatDictionary["JUST"] == "CENTER":
            justCode = 3
   
    #sets the formatWidth according to the justCode 
    if justCode == 1:
            formatWidth =right - left + 1 - 2
    else:
            formatWidth = right - left + 1
   
    #codes for the previos "JUST" key stored in oldFormat
    if oldFormat["JUST"] == "LEFT":
            oldJust = 0
    elif oldFormat["JUST"] == "BULLET":
            oldJust = 1
    elif oldFormat["JUST"] == "RIGHT":
            oldJust = 2
    elif oldFormat["JUST"] == "CENTER":
            oldJust = 3
   
    #sets the width according to the oldJust code
    if oldJust == 1:
            oldWidth = oldRight - oldLeft + 1 -2
    else:
            oldWidth = oldRight - oldLeft + 1
	     
    #start appending until an empty line("\n") or the last line in the file is found
    if line != '\n' and line is not last and line != "":
       #checks for FLOW = YES,
       if formatDictionary["FLOW"] == "YES":
            lines.append(line)
            flow = True
       elif formatDictionary["FLOW"] == "NO":
            flow = False
            nlines.append(line)
            formatPrinter(nlines, lines, left, formatWidth, justCode, flow, last)
            del nlines[:]
   
    #The following is for FLOW = YES 
    elif line == '\n' or line is last:
       if flow == True:
            #appends the empty line 
            lines.append(line)
            #joins all the elements in the array
            newline = ''.join(lines)
            formatPrinter(newline,line,left,formatWidth, justCode, flow, last)
            #following will delete all elements in list to start new paragraph
            del lines[:]
   

    #if the current line is @. CMD line, the following will clean up the previous FLOW = YES entry
    elif line == "":
       if oldFormat["FLOW"] == "YES":
            flow = True
            newline = ''.join(lines)
            formatPrinter(newline, line, oldLeft, oldWidth, oldJust,flow, last)
            del lines[:]
    

    #this will hold the last formatDictionary for use on the next iteration
    oldFormat = formatDictionary.copy()            
################ formatPrinter ####################################################################
# formatPrinter (string atString, string currline int left, int formatWidth, int justCode 
#                      boolean flow, boolean last)
# Purpose:
#           This formats the list sent in according to the variables (flow, justCode). 
#            This then prints it accordingly
# Parameters:
#  I    string	            String that holds the paragraph
#  I    string               String that holds the current line
#  I    int                 Int that holds the left margin
#  I    int                 Int that holds the right - left margins + 1 ,i.e width of line
#  I    int                 Int that holds the code for what the current JUST is in formatDictionary
#  I    boolean             bool that tells whether current flow is YES or NO
#  I    boolean             bool that tells whether the current line is the last line in file
#  O   	N/A
# Returns:
#         This function takes in the current paragraph, textwraps it according to the format Width,
#         then splits by newline. This then , line by line, proceeds to justify according to the
#         justCode sent in. It also passes in the current line to check if it happens to be the
#         last line in the file to know if it should append it or not.   
##################################################################################################
import textwrap
def formatPrinter(line, currline, left, formatWidth, justCode, flow, last):
    
    #following is for FLOW = YES 
    if flow == True:
       
        #will textwrap the line(which holds the paragraph) according to the formatWidth
        line = textwrap.fill(line, formatWidth) 
       
        #splits the line into a list by every "\n"
        newlines = line.split('\n')
		
        #following is for JUST = BULLET
        if justCode == 1:
            if line != "":
              #first line in paragraph will hold the left margin + "o "
              newlines[0] = " " * (left-1) + "o " + newlines[0]
	   #rest of the lines hold the left margin + 2 (to meet the first line)
              for i in range(1, len(newlines)):
                 newlines[i] = " " * ((left-1)+ 2) + newlines[i]
        
        #following is for every other JUST var
        else:
           for i in range(0, len(newlines)):
              #JUST = RIGHT-> right justifies each line
              if justCode == 2:
                 newlines[i] = newlines[i].rjust(formatWidth+left-1)
              #JUST = CENTER-> center justifies each line
              if justCode == 3:
                 newlines[i] = newlines[i].center(formatWidth +left+2)
              #JUST = LEFT-> manually left justifies each line with spaces
              if justCode == 0:
                 newlines[i] = " " * (left-1) + newlines[i]
        
        #checks if the current line is the last or not. 
        #reason being that if the current line is an empty line
        #then it should append to have a separator in the paragraphs
        if currline is not last:
           newlines.append(currline)
           line = '\n'.join(newlines)
           print (line, end="")
        elif currline is last: 
           line = '\n'.join(newlines)
           print (line, end="\n")
     
    #following is for FLOW = NO
    if flow == False: 
        for i in range(0,len(line)):
          #these are tests that unfortunately didn't work well
          if justCode == 2:
              line[i] = line[i].rjust(formatWidth + left -1)
          elif justCode == 3:
              line[i] = line[i].center(formatWidth + left +2)
   
          #this is for JUST = LEFT
          else:
              line[i] = " " * (left-1) + line[i]
  
          #some of the lines had \n's so this checks to see if it should add them or not
          if "\n" in line[i]:
             print (line[i], end="")
          else:
             print(line[i], end ="\n" ) 
