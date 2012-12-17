from pyparsing import Word, alphas, alphanums
from pyparsing import ZeroOrMore, OneOrMore, StringEnd, Group, srange, Forward, delimitedList, Empty, Optional, Literal, Or
from pyparsing import ParserElement, ParseException, ParseSyntaxException

import random
import time
import logging

import symbols

#import cProfile

# greet = Word( alphas ) + "," + Word( alphas ) + "!"
# greeting = greet.parseString( "Hello, World!" )
# print greeting

# By default PyParsing ignores whitespace (and strips it from its output)
# But for Story this is precious body-text which we want preserved intact.
# So we call setDefaultWhitespaceChars("") and then we define a bodytext expression which includes all non-special characters

# Tips:
#
# Because we always care about all text, we use StringEnd() to force parser to consume everything.
# If we then use only "+" to construct grammar, then the only parse error we get is the rather uninformative "Expected end of Text"
# So where possible we use the "-" operator, which is like "+" except it ALWAYS REQUIRES the elements to its right, thus throwing a parse exception closer to actual location of errors.

# 
#if __name__ == "__main__":
#    random.seed(0)  # !!! FORCE REPEATABILITY DURING TESTING


global grammar

# --------------GRAMMAR--------------

SETVARINDICATOR = "!SETVAR!"        # We insert this special indicator in the parse results to indicate when variable assignment is happening
SETCONSTINDICATOR = "!SETCONST!"    # Ditto for constant assignment

def define_grammar():
    global grammar
    def removechar(s, removechar):
        # Returns string <s> having removed all occurrences of character <removechar> 
        return ''.join(s.split(removechar))
    def removechars(s, removechars):
        for c in removechars:
            s = removechar(s, c)
        return s

    # pyparsing's setResultsName() seems to have a bug: it names both a Group AND its children with the tag, which confuses us later when we traverse the tree
    # So instead, we use setParseAction() to manually create a special tag attribute
    # This can then be read from any x=ParseResults with x.storyelement (which will be the empty string if it doesn't exist)
    def setattr_choice(this):
        this.__setattr__("storyelement","choice")
    def setattr_varlookup(this):
        this.__setattr__("storyelement","var")
    def setattr_varassign(this):
        this[0].insert(0,SETVARINDICATOR)
    def setattr_constassign(this):
        this[0].insert(0,SETCONSTINDICATOR)
                     
    # Our Grammar...
    # Returns a pyparsing object
    ParserElement.setDefaultWhitespaceChars("")
    one_expr = Forward()            # Exactly one expression (of any sort)
    one_or_more_expr = Forward()    # Compound expression (must have at least one expression)
    zero_or_more_expr = Forward()            # Compound expression (can have zero expressions)
    # Any expression which has to be forward-declared MUST then be assigned-to later using "<<" not "=" !!!
    # WARNING: You must put brackets after << otherwise the wrong thing happens. e.g. A << (B | C) without the brackets fails

    # Define allchars as all characters
    allchars = srange("[!-~]")              # All ASCII characters (same as "printables"?)
    allchars = "\t\n " + allchars           # And all whitespace chars

    # Bodytext is all characters which don't have special meaning
    bodychars = removechars(allchars, "[]{|}=<>$")   # Bodytext cannot contain special characters
    
    # Bodytext (meaning text we want to leave alone) is
    # anything consisting entirely of non-special characters
    bodytext = OneOrMore(Word(bodychars)).leaveWhitespace()    # and don't try to strip off any leading whitespace

    htmlchars = allchars
    htmlchars = removechars(htmlchars, ">")
    
    # HTML tags need to be left alone.
    htmltags = Literal("<") - ZeroOrMore(Word(htmlchars)).leaveWhitespace() - Literal(">")

    # A valid variable name starts with a letter
    varname_expr = Word(alphas,alphanums + '_')

    # A use of a variable looks like [varname]
    # varname can be computed, e.g. [{A|B}] is fine
    # but it can't be empty, e.g. []
    var_expr = Group(Literal("[") - one_or_more_expr - Literal("]")).setParseAction(setattr_varlookup)

    # A choice looks like {} or {A} or {A|B} etc.
    # (and of course A & B can be any expression at all, i.e. recursive)
    # and can include null entries e.g. {A|}
    choice_expr = (Group(Literal("{") - Group(Optional(one_or_more_expr,default=""))("firstargs") - ZeroOrMore(Literal("|").suppress() - Group(Optional(one_or_more_expr, default="")("subsargs"))) - Literal("}")).setParseAction(setattr_choice))
    # We use Optional(one_or_more_expr) rather than zero_or_more_expr because that way we can explicitly capture null expressions as choices
    # We use Group() around the subexpressions to ensure that if they are complex they don't get flattened (otherwise we'll end-up choosing from things that are just sub-lists)
    # There seems to be a but with SetResultsName() which causes both the Group and its children to both be named the same (meaning you can't tell when traversing the tree whether you are a choice group, or an element within a choice group).
    # So instead we just leave the {} brackets unsuppressed and use them as a label 

    # Setting a variable looks like [varname]={blah}
    setvar_expr = Group(var_expr + Literal("=") - choice_expr).setParseAction(setattr_varassign)
    # We use "+" (not "-") before Literal("=") because it IS legal for there to be a var_expr on it's own (i.e. when a var is used not set)

    # Using a constant looks like $const
    const_expr = Group(Literal("$") - varname_expr)

    # Setting a constant looks like $const={choice} or $const=[var]
    setconst_expr = Group(const_expr + Literal("=") - (choice_expr ^ var_expr)).setParseAction(setattr_constassign)
    # We use "+" (not "-") before Literal("=") because it IS legal when using rather than assigning a constant
    
    # Putting it all together.
    # Any earlier Forward-defined expressions must be assigned to using "<<" rather than "="
    # REMEMBER: You MUST put () around the whole RHS of << assignment, or the wrong thing happens
    one_expr << (setconst_expr | const_expr | setvar_expr | var_expr | choice_expr | htmltags | bodytext)
    # "|" is pyparsing's MatchFirst operator, so put bodytext at the end so it only gets matched if nothing else does
    # "^" is pyparsing's Or operator, which finds the longest match
    one_or_more_expr << (OneOrMore(one_expr))
    zero_or_more_expr << (ZeroOrMore(one_expr))
    zero_or_more_expr.validate() # Check for recursive loops (NOTE: Highly compute-intensive)

    final_expr = zero_or_more_expr + StringEnd()

    grammar = final_expr


# --------------EXECUTE--------------

(list_mode, execute_mode) = (1,2)


def inspectConstant(elem, mode, depth):
    # <elem> is a pyparsing.ParseResults object
    # specifically, it is a node of type "constant"
    # print "Inspect constant",elem
    if(mode==list_mode):
        return "$"+elem[1]
    else:
        return(symbols.getConst(elem[1]))

def inspectConstantAssignment(elem, mode, depth):
    # <elem> is a pyparsing.ParseResults object
    # specifically, it is a node of type "constantAssign"
    # elem[0] is the label
    # elem[1] is the variable to be assigned to: ['[','var',']'
    # elem[2] is "="
    # elem[3] is the choice to be assigned from: ['{', ['choice'], '}']
    # print "InspectConstantAssignment"
    # print "elem=",elem
    # print "elem[0]=",elem[0]
    # print "elem[1]=",elem[1]
    # print "elem[2]=",elem[2]
    # print "elem[3]=",elem[3]
    conststr = elem[1][1]
    rhs = traverseAnything(elem[3], mode, depth)
    if(mode == list_mode):
        symbols.setConst(conststr, rhs)
        return("$assign:"+conststr+"="+rhs)
    else:                   # If in execute mode, then DO interpret the choice, so it's a constant hereafter
        contents = traverseAnything(elem[3], mode, depth)   # DO interpret this here, so it's a constant!
        symbols.setConst(conststr, contents)
        return(contents)  # Constant assignment returns itself

    
def inspectVariable(elem, mode, depth):
    # <elem> is a pyparsing.ParseResults object
    # specifically, it is a node of type "variable"
    # print "Inspect variable",elem
    elem = elem[1:-1]   # Strip off the bounding []'s
    varstr = traverseAnything(elem, mode, depth+1)
    logging.debug("inspectVariable with varstr="+varstr)
    if(mode == list_mode):
        return("[lookup:"+varstr+"]")
    else:
        contents = symbols.getVar(varstr)
        logging.debug("inspectVariable seen contents = "+contents)
        result = traverseAnything(contents,mode,depth)
        logging.debug("inspectVariable returning "+result)
        return(result)

def inspectVariableAssignment(elem, mode, depth):
    # <elem> is a pyparsing.ParseResults object
    # specifically, it is a node of type "variableAssign"
    # elem[0] is the label
    # elem[1] is the variable to be assigned to: ['[','var',']'
    # elem[2] is "="
    # elem[3] is the choice to be assigned from: ['{', ['choice'], '}']
    #print "InspectVariableAssignment"
    #print elem
    #print "elem[0]=",elem[0]
    #print "elem[1]=",elem[1]
    #print "elem[2]=",elem[2]
    #print "elem[3]=",elem[3]
    varstr = traverseAnything(elem[1][1], mode, depth)
    choice = elem[3]   # Do not interpret this here, otherwise we'll just assign the variable the RESULT of the choice, not the choice itself
    symbols.setVar(varstr,choice)
    if(mode == list_mode):
        return("[assign:"+varstr+"]="+inspectChoice(choice,list_mode,depth))
    else:
        return("")  # Variable assignment doesn't result in any output


# Used for testing
(choice_random,choice_forcefirst,choice_forcelast) = (1,2,3)
choicemode = choice_random

def inspectChoice(elem, mode, depth):
    # <elem> is a pyparsing.ParseResults object
    # specifically, it is a node of type "choice"
    # print"inspect choice",elem
    elem = elem[1:-1]   # Strip off the bounding {}'s
    if(mode==list_mode):
        result = "{"
        for e in elem:
            result = result + traverseAnything(e, mode, depth+1)
            result = result + "|"
        result = result[:-1]    # Remove last |
        result = result + "}"
    else:
        if(choicemode == choice_forcefirst):
            c = 0
        elif(choicemode == choice_forcelast):
            c = len(elem)-1
        else:
            c = random.randrange(0, len(elem))
        # print "Taking choice",c,"of",len(elem),"which is",elem[c]," which is of type ",type(elem[c])
        result = traverseAnything(elem[c], mode, depth+1)
        # print "inspectChoice returning ",result
    return(result)

def traverseParseResults(tree, mode = list_mode, depth=0):
    # <tree> is a pyparsing.ParseResults object
    # if mode==list_mode then it lists all elements encountered
    # if mode==execute_mode then it executes the elements
    # print "traverseParseResults type(",type(tree),")",tree

    if(depth>100):
        assert(False)   # Catch infinite recursion early (easier debugging)

    result = ""

    if(len(tree)==0):
        # print "Empty list"
        return("")

    # print "In traverseParseResults, tree[0] ==",tree[0]
    
    if(tree[0] == "{"):
        # print "It's a choice"
        result = result + inspectChoice(tree,mode,depth+1)
    elif(tree[0] == "["):
        # print "It's a variable"
        result = result + inspectVariable(tree,mode,depth+1)
    elif(tree[0] == SETVARINDICATOR):
        # print "It's a variable assignment"
        result = result + inspectVariableAssignment(tree,mode,depth+1)
    elif(tree[0] == "$"):
        # print "It's a constant"
        result = result + inspectConstant(tree,mode,depth+1)
    elif(tree[0] == SETCONSTINDICATOR):
        # print "It's a constant assignment"
        result = result + inspectConstantAssignment(tree,mode,depth+1)
    else:   # It's a generic unlabelled tree (for example the top of the parse tree), so just traverse along the top of it
        # print "It's a list"
        for elem in tree:
            # print "traversing",type(elem),elem
            result = result + traverseAnything(elem,mode,depth+1)

    # print "traverseParseResults returning",result
    return result

def traverseAnything(thing, mode = list_mode, depth=0):
    # <thing> can be a string or a ParseResults object
    # print "traverseAnything ",type(thing),thing
    logging.debug("traverseAnything asked to traverse a "+str(type(thing)))
    if(type(thing) in (str,unicode)):
        logging.debug("it's a string")
        return(thing)
    else:
        logging.debug("it's something else")
        return(traverseParseResults(thing,mode,depth))

def do_parse(s, mode=execute_mode):
    global grammar
    logging.debug("do_parse "+s)
    try:
        #print "STORY PARSER HAS BEEN PASSED ************************"
        #print repr(s)
        #print "DONE"
        tree = grammar.parseString(s)
        #print "STORY TREE:"
        #print tree.dump()
        #print "DONE"
        result = traverseParseResults(tree, mode)
    except ParseException, e:
        result = e.pstr[0:e.loc] + e.msg.upper().replace(" ","_") + ":" + e.pstr[e.loc:]
    except ParseSyntaxException, e:
        result = e.pstr[0:e.loc] + e.msg.upper().replace(" ","_") + ":" + e.pstr[e.loc:]
    logging.debug("do_parse returning "+result)
    return result

# --------------TEST--------------
    
def run_all_tests():
    define_grammar()  # Must be done once at beginning of time (expensive operation)

    verbose = True

    def test(s, desiredA=None, desiredZ=None):
        # s is an input string which is parsed, then listed, then executed twice.
        # the first execution is with all choices forced to be the first choice (hard to test things with randomness!) and must match desiredA
        # the second execution is with all choices forced to be the last choice and must match desiredZ
        global choicemode
        if((desiredA!=None) and (desiredZ==None)):
            desiredZ = desiredA     # If no second arg, then is same as first
        if(verbose):
            print "\nTEST:\""+s+"\""
        while(1):
            if 1:   # Let do_parse catch exceptions for us
                if(verbose):
                    print "LIST:\""+do_parse(s,mode=list_mode)+"\""
                if(desiredA!=None):
                    choicemode = choice_forcefirst
                else:
                    choicemode = choice_random
                resultA = do_parse(s,mode=execute_mode)
                if(verbose):
                    print "EXEC:\""+resultA+"\""
                if(desiredA!=None):
                    choicemode = choice_forcelast
                resultZ = do_parse(s,mode=execute_mode)
                if(verbose):
                    print "EXEC:\""+resultZ+"\""
                choicemode = choice_random  # Restore the global variable to random!
                if(desiredA!=None):
                    assert(resultA==desiredA)
                if(desiredZ!=None):
                    assert(resultZ==desiredZ)
            else:
                tree = grammar.parseString(s)
                print tree.dump()
                listresult = traverseParseResults(tree, mode=list_mode)
                print "LIST:\""+listresult+"\""
                execresult = traverseParseResults(tree, mode=execute_mode)
                print "EXEC:\""+execresult+"\""
            if(verbose):
                i = raw_input()
                if(i==""):
                    return
            else:
                return

    verbose = True

    test("""

[adverb]={happily|sadly|emotionally|doubtfully|artfully|slyly|loudly|softly|stupidly|stoically|carefully|delicately|artfully}
[chemical.atom]={antimony|arsenic|aluminum|selenium|hydrogen|oxygen|nitrogen|rhenium|nickel|neodymium|neptunium|germanium|iron|americium|ruthenium|uranium|europium|zirconium|lutetium|vanadium}
[chemical.ending]={glyceride|oxide|hydrogenate|permangenate|selenide|chloride|hydroxide}
[chemical]={[chemical.atom]{| {|mono-|di-|tri-}[chemical.ending]}}
[container]={flask|flagon|barrel|test-tube|pipette|water-butt|watering can|jug|glass|cup|beaker|gasoline can}


[digit]={0|1|2|3|4|5|6|7|8|9}
[digit1_9]={1|2|3|4|5|6|7|8|9}
[digit2_9]={2|3|4|5|6|7|8|9}
[digit.recurse]={|[digit]|[digit][digit.recurse]}
[digit.any]={[digit1_9][digit.recurse]}
[digit.plural]={[digit2_9]|[digit1_9][digit.recurse]}
digit.any: [digit.any] [digit.any] [digit.any] [digit.any] [digit.any] [digit.any] [digit.any] [digit.any]
digit.plural: [digit.plural] [digit.plural] [digit.plural] [digit.plural] [digit.plural] [digit.plural] [digit.plural]

[number1_9]={one|two|three|four|five|six|seven|eight|nine}
[number10_19]={ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen}
[number.20_90]={twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety}
[number.20_99]={[number.20_90]{|-[number1_9]}}
[number1_99]={[number1_9]|[number10_19]|[number.20_99]}
[number1_999]={{|[number1_9] hundred and }[number1_99]}
[number1_9999]={{|[number1_9] thousand }[number1_999]}
number1_99: [number1_99] [number1_99] [number1_99] [number1_99] [number1_99] [number1_99] [number1_99] [number1_99] [number1_99] [number1_99]
number1_999: [number1_999],[number1_999],[number1_999],[number1_999],[number1_999],[number1_999],[number1_999],[number1_999]
number1_9999: [number1_9999],[number1_9999],[number1_9999],[number1_9999],[number1_9999],[number1_9999],[number1_9999],[number1_9999]

[letter]={A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z}

[adj.colour]={red|orange|yellow|green|blue|indigo|violet}
[adj.texture]={striped|ribbed|rippled|rough|smooth|spotted|blotchy|splattered}
[adj.size]={miniscule|teeny|tiny|small|normal-sized|big|large|huge|enormous}
[adj.temperature]={frozen|cold|hot|boiling}
[adj.senses]={smelly|fragrant|pungent|tasty|sweet|sour|salty|bitter|smooth|rough|prickly|oily|metallic|bright|dim|ugly|beautiful}
[adj.emotion]={happy|sad|depressed|excited|bored|expectant|dead|alive|annoying|irritating}
[adj.moving]={drifting|halting|slow|fast|speedy|zooming|braking|accelerating}
[adj.disgusting]={wormy|sticky|smelling|farting|oozing|pussy|rotten|mouldy|stinking}
[adjective]={[adj.size]|[adj.temperature]|[adj.senses]|[adj.emotion]|[adj.moving]|[adj.disgusting]|[adj.colour]|[adj.texture]}

[furniture.street]={lamppost|postbox|traffic light|shop|zebra crossing}
[furniture.home]={sofa|TV|chair|bed|table|lamp}
[furniture]={[furniture.home]|[furniture.street]}

[animal]={Aardvark|Anteater|Cat|Goat|Horse|Iguana|Pig|Porcupine|Puppy|Zebra}
[vegetable]={aubergine|zucchini|cucumber|apple|marrow|onion|mushroom}
[inanimate]={rock|car|[furniture]}
[thing]={[animal]|[vegetable]|[inanimate]}

[profession]={gamekeeper|butcher|fireman|accountant|lawyer|engineer}
[relative.immediate]={father|mother|son|daughter}
[relative.oneremove]={niece|nephew|uncle|aunt}
[relative.proximate]={{[relative.immediate]|[relative.oneremove]}{| in-law}}
    
[greats]={||great {||[greats]}}
[relative]={[greats]{|grand}[relative.proximate]}

# Valid Roman numerals
[roman0to9]={|I|II|III|IV|V|VI|VII|VIII|IX} # There is no symbol for zero!
[roman0to9] [roman0to9] [roman0to9] [roman0to9] [roman0to9]
[roman10to19]={X[roman0to9]}
[roman10to19] [roman10to19] [roman10to19] [roman10to19] [roman10to19]
        

[adverb.mild]={mildly|slightly|somewhat|tentatively|a bit}
[adverb.extreme]={totally|utterly|amazingly|completely|famously|notoriously}
[adverb]={[adverb.mild]|[adverb.extreme]}
[sometimes]={sometimes|occasionally|often|inevitably|then}
[Iam]={I am|You are|He is|She is|We are|They are}
[Iwas]={I was|You were|He was|She was|We were|They were}
[willbe]={will be|might be|could be|could have been|would have been|might have been|became|wanted to be}
[pron]={I|You|He|She|We|They}
[Iwillbe]={[pron] [willbe]}
[pron_phrase]={[Iam]|[Iwas]|[Iwillbe]}

[consonant.uppercase]={B|C|D|F|G|H|J|K|L|M|N|P|Q|R|S|T|V|W|X|Y|Z}
[consonant.lowercase]={b|c|d|f|g|h|j|k|l|m|n|p|q|r|s|t|v|w|x|y|z}
[vowel.uppercase]={A|E|I|O|U}
[vowel.lowercase]={a|e|i|o|u}

[name.boy]={Fred|Jim|John|Matt|Mike|Tim|Tom|William}
[name.girl]={Amanda|Freya|Laura|Mandy|Nikki|Tanya}
[nonvowelsounds]={[consonant.lowercase]|th|sh|ll|cl|ff|sl}
[name.imaginary]={[consonant.uppercase][vowel.lowercase][nonvowelsounds][vowel.lowercase][nonvowelsounds]}
[name]={[name.boy]|[name.girl]|[name.imaginary]}

[bodilyfunction]={crying|laughing|giggling|farting|belching|weeing|pooing}
[adjacentto]={near|over|under|beside|in close proximity to|against}
[proximity]={nearby|far away|local|neighbourhood|county|national}
[dwelling]={house|church|shop|cave|hostel|hotel|butchers shop|sweet shop|department store}
[action.human]={rub|stretch|yawn|bend over}

[villagename.prefix]={Little|Greater|Upper|Lower|Old|New}
[villagename.main]={Snuff|Cam|Ox|Hitch|Steven|Roys|Sud|New|Alde|Letch|Edin|Car|Hat|Man|Birming|Glou|Lei|Bal|Mel|Upping|Rug|Har|Chelms|Wal}
[villagename.suffix]={ton|ington|bridge|ford|in|age|bury|bry|by|market|burgh|worth|field|gow|diff|ham|chester|cester|dock|dreth|row|den}
[villagename.s2]={-by-sea|-on-the-Naze|-in-the-Wold}
[villagename.x]={[villagename.main][villagename.suffix]}
[villagename]={[villagename.x]|[villagename.prefix] [villagename.x]|[villagename.x][villagename.s2]}
[villagename]

[crime.intro]={Conspiring to commit|Aiding and Abetting}
[crime.major]={assault|arson|bribery|burglary|card fraud|taking a motor|handling stolen goods|theft|being in a possession of a firearm|drug cultivation|embezzlement|extortion|forgery|fraud|indecent exposure|kidnapping|manslaughter|murder|prostitution|shoplifting}
[crime.minor]={cycling the wrong way up a one-way street|disturbing the peace|exceeding the speed limit}
[crime.mod]={without {permission|consent}|with a minor|in a built-up area|after the hours of darkness}
[crime]={{|[crime.intro] }{[crime.major]|[crime.minor]}{| [crime.mod]}}
[crime]

[cereal.manufacturer]={Waitrose|Kelloggs|Lidl}
[cereal.first]={Weeta|Crunchy |Honey |Sugar |Fruit |Rice |Corn |Oat |Shredded }
[cereal.last]={Bix|Nutz|Puffs|Loops|Crispies|Flakes|Wheat}
[cereal]={[cereal.first][cereal.last]}
New $cereal=[cereal]! They're [adjective], they're [adjective], they're just so [adjective]!.

Put 2ml [chemical] into a [container]. [adverb] add 5ml [chemical] {| and {duck|take cover|flee for your life|light the blue touch paper|retire immediately}}

The number plate was [letter][letter][digit][digit] [letter][letter][letter]

$plur={one|many}
[numplur.one]={a}
[numplur.many]={no|[digit.plural]} # Zero or many need to be pluralised
[s.one]={}
[s.many]={s}
Happiness is...[numplur.$plur] [adjective] [thing][s.$plur]

[orks]={stalks|talks|baulks}
[arfs]={laughs|barfs}
[ieves]={believes|deceives}
My [relative] [$rhyme={orks|arfs|ieves}], my [relative] [$rhyme], oh what a family are we.

[phrase]={[pron_phrase] {|[adverb] }$repeat=[adjective]{||| - [sometimes]}{|||, oh so $repeat}{.|.|.|.|.|?|!}}
{LIFE|DEATH|ENNUI|LESSONS} - a poem
[phrase]
[phrase]
[phrase]
             
Directions to the $dest={airport|public loo|swimming pool|police station|local crack house}
[directions]={{Proceed|Go} {|East |West |North |South }{|100 yards }{up|down|along} {{the {street|road|path|track|lane|motorway}|a small pipe}}{| until you reach the {pub|crossroads|T-junction|brothel|road {|signed }to [villagename]}}.}
[directions]
Turn {right|left}.
[directions]
You have reached the $dest. 

This is a test of constant assignment. The number is $number={1|2|3} and the letter is [letters]={A|B|C}$letter=[letters]. Yup, that's right, $number and $letter.
This is a test of variable assignment. [var]={X|Y|Z}The letter is [var]. Now [var]. Now [var].

[Gender.boy]={He}
[Gender.girl]={She}
[gender.boy]={he}
[gender.girl]={she}
[prop.boy]={his}
[prop.girl]={her}
[boy.other]={girl}
[girl.other]={boy}
Once there was a $gender={boy|girl} called $name=[name.$gender]. $He=[Gender.$gender] was {sometimes|often} to be found [adverb] [bodilyfunction] [adjacentto] the {city|town|village|asylum} [furniture.street].
Often as $he=[gender.$gender] went into $his=[prop.$gender] $dwelling=[dwelling], $he used to [action.human] against $his [furniture.home].
$He made friends with a [adjective] [$gender.other] called [name.imaginary].
When I grow up, I want to be a $job={builder|baker|candlestick maker}, because {|I hear that |the rumour is that }{$job}s get paid a lot.

[digit] green bottles...
[var]={A|B}[var][var][var][var][var][var][var][var][var][var]
[v2]={[var]|C|D}[v2][v2][v2][v2][v2][v2][v2][v2][v2][v2][v2][v2]

[name.imaginary] [name.imaginary] [name.imaginary] [name.imaginary] [name.imaginary]

[film.firstpart]={Indiana Jones and|The Spy who Loved|The Meaning of|The Life of|You only live}
[film.secondpart]={the Temple of Doom|Me|Life|Brian|Twice}
[film]={[film.firstpart] [film.secondpart]}
[star.firstname]={James|Austin|Mickey|Donald|Jason|Lisbeth}
[star.lastname]={Bond|Powers|Mouse|Duck|Bourne|Salander}
[star]={[star.firstname] [star.lastname]}
'I have seen [star] in "[film]" [number1_99] times', said [name], $adverb=[adverb], [bodilyfunction] [adjacentto] the [furniture.street]. Why $adverb? Who knows?

[weapon]={gun|atomic blaster|slingshot|peashooter|siege engine|nuclear weapon}

This is a story about a $gender={boy|girl} called $name=[name.$gender].
$he=[gender.$gender] lived in a [adjective] [dwelling] in the heart of $village=[villagename].
Early one morning, $name got up, ate a [container] of [cereal] (rich in [chemical]).
$he lay on the [furniture.home] [bodilyfunction] for a while, then decided to commit $crime=[crime].

Now, $crime had never been committed in $village before.
But [adverb] $name had a [weapon] with which to do it.
He consulted his friend $friend={[adjective] [name]}, who lived in the [proximity] [building].
'I have seen [star] in "[film]" [number1_99] times', boasted friend, $adverb=[adverb], [bodilyfunction] [adjacentto] the [furniture.street].


[headadj]={{|[headadj] }Metric|EU|Fat|French|Feral|Gypsy|Immigrant|Muslim|Polish|Celebrity|Unemployed}
[subhead]={[headadj] {MMR jabs|grannies|children|neighbours} {steal your {identity|daughters}|raise energy prices|{cause|cure|infect you with} {cancer|diabetes|AIDS|Swine flu}|{raise|cut} house prices}}
[headline.dailymail]={Do [subhead]?|[subhead]!}

[headline.dailymail]
[headline.dailymail]
[headline.dailymail]
[headline.dailymail]
[headline.dailymail]
[headline.dailymail]
[headline.dailymail]
[headline.dailymail]
[headline.dailymail]



""")

    if 1:
        # Tests of Constants
        test("$fred={1|2|3}$fred$fred","111","333")   # Constant definition and use
        test("$fred ","3 ")
        test(" $fred"," 3")
        test(" $fred "," 3 ")
        test("The number is $fred={one|two|three}. Yes, $fred.","The number is one. Yes, one.","The number is three. Yes, three.")
        test("$fred={{1|2}|{3|4}} $fred $fred $fred")   # Even deep hierarchical choices should remain constant (i.e. be fully-evaluated to strings)
        test("[joe]={1|2} $fred={{[joe]|2}|{3|4}} $fred $fred $fred $fred $fred")
        test("[var]={1|2|3}","","")
        test("$fred=[var]","1","3")
        test("The number is $fred. Yes, $fred.","The number is 3. Yes, 3.", "The number is 3. Yes, 3.")
        test("$fred={a|b|c}","a","c")
        test("$fred {$fred} {yes $fred}")

    if 1:
        test("{}","")
        test("{ }"," ")
        test("pre{}","pre","pre")
        test("{}post","post","post")
        test("{A}","A","A")
        test("{A|B}","A","B")
        test("{A|}","A","")
        test("{|A}","","A")
        test("{|}","","")
        test("{A|B|C}","A","C")
        test("[var]={V}","","")
        test("{A|[var]B}","A","VB")
        test("{A|{B|C}}","A","C")
        test("{A[var]}","AV","AV")
        test("[var]","V")
        test("[var]={}","")
        test("[var]={A}","")
        test("[var]={A|B}","")
        test("pre[var]={A|B}","pre",)
        test("[var]={A|B}post","post")
        test("<bloggs>","<bloggs>")
        test("fred","fred")
        test("fred<bloggs>","fred<bloggs>")
        test("<bloggs>fred","<bloggs>fred")
        test("fred<a html=foo>", "fred<a html=foo>")   # Test that special (to us) characters inside HTML tags are ignored
        test("<h1><a class=\"wikiword\" href=\"/story\">story</a></h1><p>Hello</p>","<h1><a class=\"wikiword\" href=\"/story\">story</a></h1><p>Hello</p>")
        test("oneline\ntwoline","oneline\ntwoline")
        test("<h1><a class=\"wikiword\" href=\"/MainPage\">MainPage</a></h1><p>I think we can edit this page. <strong>Groovy</strong>!</p><p>Here are some <a class=\"wikiword\" href=\"/Ideas\">Ideas</a>.</p><p><img src=\"http://beart.org.uk/Resume/index_files/image027.jpg\" onmouseover=\"undefined\" onmouseout=\"undefined\" alt=\"\" title=\"undefined\" width=\"78\" height=\"71\" />&nbsp;</p><p>Here is the index:</p><p><ul><li><a class=\"wikiword\" href=\"/MainPage\">MainPage</a></li><li><a class=\"wikiword\" href=\"/Ideas\">Ideas</a></li><li><a class=\"wikiword\" href=\"/story\">story</a></li><li><a class=\"wikiword\" href=\"/flumbucket\">flumbucket</a></li></ul></p><p>&nbsp;</p><p>&nbsp;</p>","<h1><a class=\"wikiword\" href=\"/MainPage\">MainPage</a></h1><p>I think we can edit this page. <strong>Groovy</strong>!</p><p>Here are some <a class=\"wikiword\" href=\"/Ideas\">Ideas</a>.</p><p><img src=\"http://beart.org.uk/Resume/index_files/image027.jpg\" onmouseover=\"undefined\" onmouseout=\"undefined\" alt=\"\" title=\"undefined\" width=\"78\" height=\"71\" />&nbsp;</p><p>Here is the index:</p><p><ul><li><a class=\"wikiword\" href=\"/MainPage\">MainPage</a></li><li><a class=\"wikiword\" href=\"/Ideas\">Ideas</a></li><li><a class=\"wikiword\" href=\"/story\">story</a></li><li><a class=\"wikiword\" href=\"/flumbucket\">flumbucket</a></li></ul></p><p>&nbsp;</p><p>&nbsp;</p>")
        test(u"",u"",u"")
        test(u"unicode",u"unicode")
        test(u"[unicode_var]={A}",u"")
        test("pre[var]","preA","preB")
        test("[var]post","Apost","Bpost")
        test("[A]={VariableA}[B]={VariableB}","","")
        test("[{A|B}]","VariableA","VariableB") # Even the name of a variable can be dynamic
        test("","")
        test(" "," ")
        test("a","a")
        test(".",".")
        test("Hello","Hello")
        test(" Hello"," Hello")
        test("Hello ","Hello ")
        test(" Hello "," Hello ")
        test("Hello there","Hello there")
    # Tests that should fail
    if 1:
        test("[x]={error location","[x]={error locationEXPECTED_\"}\":")
        test("[]","[EXPECTED_\"$\":]")     # Missing variable name
        test("[ ]","[NO_SUCH_VARIABLE: ]")    # Whitespace variable name
        test("[hell","[hellEXPECTED_\"]\":")  # Missing ]
        test("o]","oEXPECTED_END_OF_TEXT:]")     # Missing [
        test("|","EXPECTED_END_OF_TEXT:|")      # Missing []
        test("[var]=","[var]=EXPECTED_\"{\":")  # Missing rhs of assignment
        test("[var]=x","[var]=EXPECTED_\"{\":x")  # Missing rhs of assignment
        test("[var]={","[var]={EXPECTED_\"}\":") # Missing }
        test("[var]=[var]","[var]=EXPECTED_\"{\":[var]") # Wrong type of rhs
        test("$","$EXPECTED_W:(ABCD...,ABCD...):")   # Missing constant name
        test("$[]","$EXPECTED_W:(ABCD...,ABCD...):[]")
        test("${}","$EXPECTED_W:(ABCD...,ABCD...):{}")
        test("$fred=","$fred=EXPECTED_\"{\":")  # Missing constant assignment

    print "Tests Complete"

if __name__ == "__main__":
    run_all_tests()
    # cProfile.run('run_all_tests()')
