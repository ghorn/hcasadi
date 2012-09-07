import casadi
import string
import sys

x = casadi.ssym('x')

f = casadi.SXFunction([x],[x])

typeNameToHS = { 'OT_STRING':'String',
                 'OT_BOOLEAN':'Bool',
                 'OT_JACOBIANGENERATOR':None,
                 'OT_STRINGVECTOR':'[String]',
                 'OT_INTEGER':'Int',
                 'OT_SPARSITYGENERATOR':None,
                 'OT_REAL':"Double",
                 'OT_VOIDPTR':None,
                 'OT_FX':None
#                 'OT_UNKNOWN':None
                 }

def toHsType(name, g):
    optionTypeName = g.getOptionTypeName(name)
    if optionTypeName in typeNameToHS:
        hsType = typeNameToHS[optionTypeName]
        if hsType != None:
            desc = g.getOptionDescription(name)
            default = "???"
            try:
                default = str(g.getOptionDefault(name))
            except:
                pass
            return string.capitalize(name) + " " + hsType + " -- ^ (" +default+") "+desc
        else:
            return None
    else:
        print "ERROR: type \"%s\" not handled (field: \"%s\")" % (optionTypeName,name)
        sys.exit(1)

def writeADT(dataName, callerName, ptrName, g):
    adtLines = ["data "+dataName+" = "]
    callerLines = [callerName+" :: "+ptrName+" -> "+dataName+" -> IO ()"]
    firstLine = True
    for name in g.getOptionNames():
        hsType = toHsType(name,g)
        if hsType != None:
            if firstLine:
                adtLines.append("    "+hsType)
                firstLine = False
            else:
                adtLines.append("  | "+hsType)
            callerLines.append(callerName+" fun ("+string.capitalize(name)+" x) = unsafeSetOption' fun \""+name+"\" x")
    return (adtLines, callerLines)

sxs = writeADT("SXFunctionOption","sxFunctionUnsafeSetOption","SXFunction",f)
for line in sxs[0]:
    print line
print ""
for line in sxs[1]:
    print line
print ""

s = casadi.IpoptSolver(f)
nlps = writeADT("NLPSolverOption","nlpSolverUnsafeSetOption","NLPSolver",s)
for line in nlps[0]:
    print line
for line in nlps[1]:
    print line

