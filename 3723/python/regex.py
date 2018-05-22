import re
text = "please call 343-899-9299"
phone = re.compile(r'\d{3}-\d{3}-\d{4}')
match = phone.search(text)
if match != None:
    print (match.group())

else:
    print ("not found")




