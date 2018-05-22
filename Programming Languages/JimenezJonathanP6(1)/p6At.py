import re
################ setVariable ####################################################################
# setVariable (string atString, dictionary varDictionary)
# Purpose:
#		Reads the string with a varName and value and places them into the dicionary   
#		that was passed in.
# Parameters:
#  I    string				String that holds a varName and its value separated by and "="
#  I    dicionary			Dictionary that holds every varName : value
#  O   	N/A
# Returns:
#		N/A
##################################################################################################
def setVariable(atString, varDictionary):
     
    #Regex that matches the varName and value
    varRe = re.compile(r'@(\w*)=(\".*\"|\w*)')
    #Checks if atString matches the regular expression to continue or print error
    if varRe.match(atString):
       match = varRe.search(atString)
    else:
       print("*** Invalid varName, found:", atString)  
       return None
    #Separating the varName and value into 2 variables
    varName = match.group(1)
    value = match.group(2)
	
    #Checks to see if the value has quotes in them. If so, uses regex to separate the value from them   
    if '"' in value:
       quoteRe = re.compile(r'\"(.*)\"')
       value = quoteRe.search(value)
       varDictionary[varName] = value.group(1)
    else:
       varDictionary[varName] = value
################ setFormat ####################################################################
# setVariable (string atString, dictionary formatDictionary)
# Purpose:
#		Reads the string with a varName and value and places them into the dicionary   
#		that was passed in.
# Parameters:
#  I    string				String that holds a format and its value separated by and "="
#  I    dicionary			Dictionary that holds every format : value
#  O   	N/A
# Returns:
#		N/A
##################################################################################################
def setFormat(atString, formatDictionary):
    
    formatRe = re.compile(r'(\w*)=(\w*|\d*)')
    if formatRe.match(atString):
       matched = True
    else:
       print ("*** Invalid format, found: ", atString)
       return None 
    #Array that holds the possible formats to match for error checks
    formatNames = ["JUST", "RM", "LM", "FLOW", "BULLET"]
    match = re.split("[\s,=]\s*", atString)
    
    #Separating the split formats and values into 2 arrays
    formats = match[::2]
    values = match[1::2]
    
    #Checking if there is a whitespace as the last element and deleting if so
    if formats[-1] == "":
       formats.pop()
	
    #Iterating through every key in the dictionary   
    for key in formatDictionary:
      for i in range(0,len(formats)):
	 #Checks if formats is FLOW and matches key. 
         if key == formats[i] and key == "FLOW":
           if values[i] == "YES" or values[i] == "NO":  
              formatDictionary[key] = values[i]
           else: 
              print ("*** Bad value for", formats[i], " found:" ,values[i])
	 #Checks if formats is RM and matches key.
         elif key == formats[i] and key == "RM":
           if values[i].isdigit():
              formatDictionary[key] = values[i]
           else:
              print ("*** Bad value for", formats[i], " found:" , values[i])		  	    
         #Checks if formats is RM and matches key.
         elif key == formats[i] and key == "LM":
           if values[i].isdigit():
              formatDictionary[key] = values[i]
           else:
              print ("*** Bad value for", formats[i], " found:" ,values[i])
	 #Checks if formats is RM and matches key.
         elif key == formats[i] and key == "JUST":
           if values[i] == "LEFT" or values[i] == "RIGHT" or values[i] == "CENTER" or values[i] == "BULLET":
              formatDictionary[key] = values[i]
           else:
              print ("*** Bad value for", formats[i], " found:" ,values[i])
    #Checks if format is one fo the possible formats. Prints error if not. 
    if formats[i] not in formatNames:      
         print("*** Invalue format, found: ", end="")
         print(formats[i],values[i],sep="=")
