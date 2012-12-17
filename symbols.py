# Symbols
# Sets and Gets the values of symbols (variables and constants)

import logging

storyconsts = {}    # The dict where we hold the story's constants
storyvars = {}      # The dict where we hold the story's dynamic variables

getVar_callback = None  # If defined (by calling install_getVar_callback)
                        # then this is called as a last ditch attempt
                        # to lookup a variable name (if no name is defined locally)

def getConst(conststr):
    # print "getConst "+conststr
    if(conststr in storyconsts.keys()):
        return(storyconsts[conststr])
    else:
        return("NO_SUCH_CONSTANT:"+conststr)

def setConst(conststr, contents):
    # print "set constant "+conststr+" to \""+contents+"\""
    storyconsts[conststr]=contents

    
def getVar(varstr):
    global getVar_callback
    if(varstr in storyvars.keys()):
        return(storyvars[varstr])
    else:
        if(getVar_callback != None):
            result = getVar_callback(varstr)
            if(result != None):
                logging.debug("getVar("+varstr+") returning "+result)
                return(result)
        return("[NO_SUCH_VARIABLE:"+varstr+"]")

def setVar(varstr,contents):
    storyvars[varstr]=contents


def install_getVar_callback(gvc):
    global getVar_callback
    getVar_callback = gvc
