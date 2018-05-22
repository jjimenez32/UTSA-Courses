import re

#list that holds flow = yes lines
lines = list()
#list that holds flow = no lines
nlines = list()
#used to try and identify the current line as a line with @. CMD 
oldFORMAT = {"JUST": "LEFT", "BULLET":"o", "RM":"80","LM":"1","FLOW":"YES"}

################ varLine ####################################################################
# varLine (string line, dictionary varDictionary)
# Purpose:
#             Checks if line has a @ variable name and replaces it with the VAR dictionary key
# Parameters:
#  I    string	                    String that holds the current line of the file
#  I    dicionary                   Dictionary that holds every varName : value
#  O   	N/A
# Returns:
#       out/line                    String with replaced variable names
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
#       **NOTE**: could not figure out how to read and identify a line with a @. CMD immediately 
#       after a string. For that, It would unfortunately continue adding to the list.
#       Will only work if next line was an empty line or the last line. Please have mercy :(
##################################################################################################
def formatLine(line, formatDictionary,last):
    #typecasting the formatDictionary LM and RM to ints to use 
    left = int(formatDictionary["LM"])
    right = int(formatDictionary["RM"])
    
    #following was to try to check if the formatDictionary was changed ,i.e. if the line is 
    #a line with @. CMD.  
    global oldFORMAT
    if oldFORMAT == formatDictionary:
          changed = False
    if oldFORMAT != formatDictionary:
          changed = True
		  
    #following will append to list if flow is yes
    if formatDictionary["FLOW"] == "YES":
         flow = True
         
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
	     
          #start appending until an empty line("\n") or the last line in the file is found
         if line != '\n' and line is not last:
            lines.append(line) 
         elif line == '\n' or line is last or changed == True or line == None:
            #appends the empty line
            lines.append(line)
            #joins all the elements in the array
            newline = ''.join(lines)
            formatPrinter(newline,line,left,formatWidth, justCode, flow, last)
            #following will delete all elements in list to start new paragraph
            del lines[:]
			
    #following will append to list if flow is yes     
    elif formatDictionary["FLOW"] == "NO":
         flow = False

         if formatDictionary["JUST"] != "BULLET":      
            formatWidth = right - left + 1
            #since it just wants the lines printed, the printer will print line  by line 
            #so it appends and immediately sends to printer instead of waiting for emptyline
            if line != '\n' or line is not last:
               #appends line to list
               nlines.append(line)
               justCode = 0
               formatPrinter(nlines, lines,left,formatWidth, justCode, flow, last)
               #following will delete the elements to start new paragraph
               del nlines[:]
    oldFORMAT = formatDictionary.copy()            
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
#		N/A 
##################################################################################################
import textwrap
def formatPrinter(line, currline, left, formatWidth, justCode, flow, last):
     if flow == True:
        
        #will textwrap the line(which holds the paragraph) according to the formatWidth
        line = textwrap.fill(line, formatWidth) 
        #splits the line into a list by every "\n"
        newlines = line.split('\n')
		
        #following is for JUST = BULLET
        if justCode == 1:
           #first line in paragraph will hold the left margin + "o "
           newlines[0] = " " * left + "o " + newlines[0]
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
        if currline is not last:
           newlines.append(currline)
           line = '\n'.join(newlines)
           print (line, end="")
        elif currline is last: 
           line = '\n'.join(newlines)
           print (line, end="\n")

     if flow == False: 
        #this will cleanup any previoous flow = yes if empty line or last line was not found
        for i in range(0,len(currline)):
          currline[i] = " " * (left-1) + currline[i]
          print (currline[i],end ="")
          del currline[:] 
		
        #format and print the line the same way as left justify
        for i in range(0,len(line)):
          line[i] = " " * (left-1) + line[i]
          if "\n" in line[i]:
             print (line[i], end="")
          else:
             print(line[i], end ="\n" )
         
        
        
      
