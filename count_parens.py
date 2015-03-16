def count_parentheses(text):
    lcount = 0
    rcount = 0
    index = 0
    for letter in text:
	if letter == "(":
	    lcount += 1
	elif letter == ")":
	    rcount += 1
	if rcount > lcount:
	    print "found an extra ')' at {0}".format(str(index))
	    return
	index += 1
    if lparens > rparens:
        print "you're missing {0} ')'".format(lparens - rparens)
    else:
        print "your parentheses are balanced"
        
