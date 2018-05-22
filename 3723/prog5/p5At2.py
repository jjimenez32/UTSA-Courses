import re
#
#
#
#
#
#
#
#
#
#
def setVariable(atString, varDictionary):
    varNames = ["first","last","petName","street","city","state","zip","title"]
    varRe = re.compile(r'@(\w*)=(\".*\"|\w*)')
    match = varRe.search(atString)
    varName = match.group(1)
    value = match.group(2)
    if varName not in varNames:
       print ("*** Invalid varname found: ",end="")
       print (varName,value,sep="=")
    elif '"' in match.group(2):
       quoteRe = re.compile(r'\"(.*)\"')
       value = quoteRe.search(value)
       varDictionary[varName] = value.group(1)
    else:
       varDictionary[varName] = value
#
#
#
#
#
#
#
#
def setFormat(atString, formatDictionary):
    formatNames = ["JUST", "RM", "LM", "FLOW", "BULLET"]
    match = re.split("[\s,=]\s*", atString)
    cmd = match[::2]
    ans = match[1::2]
    if cmd[-1] == "":
       cmd.pop()
    for key in formatDictionary:
      for i in range(0,len(cmd)):
         if key == cmd[i] and key == "FLOW":
           if ans[i] == "YES" or ans[i] == "NO":  
              formatDictionary[key] = ans[i]
           else: 
              print ("*** Bad value for", cmd[i], " found:" ,ans[i])
         elif key == cmd[i] and key == "RM":
           if ans[i].isdigit():
              formatDictionary[key] = ans[i]
           else:
              print ("*** Bad value for", cmd[i], " found:" , ans[i])
         elif key == cmd[i] and key == "LM":
           if ans[i].isdigit():
              formatDictionary[key] = ans[i]
           else:
              print ("*** Bad value for", cmd[i], " found:" ,ans[i])
         elif key == cmd[i] and key == "JUST":
           if ans[i] == "LEFT" or ans[i] == "RIGHT" or ans[i] == "CENTER" or ans[i] == "BULLET":
              formatDictionary[key] = ans[i]
           else:
              print ("*** Bad value for", cmd[i], " found:" ,ans[i])
    if cmd[i] not in formatNames:      
        print ("*** Invalid format found: ",end='')
        print (cmd[i],ans[i],sep='=')



