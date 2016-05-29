import os, math, algorithm, macros, sets,strutils, tables, intsets, fastentries

# Execute a shell command
proc shell*(cmd: string) =
    if os.execShellCmd(cmd) != 0:
       raise newException(EOS, cmd & " returned error code 1")

# Determine whether a file exists
proc fexists*(fname: string) : bool =
    try: discard Open(fname)
    except EIO: return false
    return true

# Count the number of lines in a file with a standard Nimrod util
proc lineCount*(fname: string) : int =
    result = 0
    for line in lines(fname):
        result += 1

# Count the number of lines ina  file using my fastentries module
proc lineCountFast*(fname: string) : int =
    result = 0
    for line in entriesMapped(fname):
        result += 1

# Compile Nimrod files within a Nimrod script
proc nim*(fname: string, nimParams = "--cincludes:. --opt:speed c -r --parallelBuild:1") =
    shell "nimrod " & nimParams & " " & fname & " 2>&1 >> log.txt"

# Like enumerate in Python over file lines
iterator pairs*(f: TFile) : tuple[key: int, val: TaintedString] =
    var i = 0
    for line in lines(f):
        yield (i,line)
        i += 1

# A handy debug command that uses the AST to show variable names
# Inspired by http://nimrod-lang.org/tut2.html
macro vecho*(n: varargs[expr]): stmt =
  result = newNimNode(nnkStmtList, n)
  for i in 0..n.len-1:
    result.add(newCall("write", newIdentNode("stdout"), toStrLit(n[i])))
    result.add(newCall("write", newIdentNode("stdout"), newStrLitNode(": ")))
    result.add(newCall("write", newIdentNode("stdout"), n[i]))
    if i != n.len-1:
        result.add(newCall("write", newIdentNode("stdout"), newStrLitNode(", ")))
  result.add(newCall("writeln", newIdentNode("stdout"), newStrLitNode("")))

# Convert an array into a string
proc `$`*[T](s: openarray[T]) : string =
    result = ""
    let n = s.len
    var idx = 1
    for i in s:
        result &= $i
        if idx != n: result &= ","
        idx += 1

# Convert a Nimrod-like matrix into a string
proc `$`*[T](s: seq[seq[T]]) : string =
    result = ""
    for x in s:
        result &= $x
        result &= "\n"

# Convert a sequence to a set
proc toSet*[T](s: seq[T]) : TSet[T] =
    result = initSet[T]()
    for x in s: result.incl(x)

# Convert a sequence to an integer set
proc toIntSet*(s: seq[int]) : TIntSet =
    result = initIntSet()
    for x in s: result.incl(x)

# Convert a set into a sequence
proc toSeq*[T](A: TSet[T]) : seq[T] =
    result = @[]
    for x in A.items: result.add(x)

# Set subtraction
proc `-`*[T](A: TSet[T], B: TSet[T]) : TSet[T] =
    result = initSet[T]()
    for x in A.items:
        if not B.contains(x): result.incl(x)

# Set intersection
# TODO: Use type classes
proc `*`*[T](A: TSet[T], B: TSet[T]) : TSet[T] =
    result = initSet[T]()
    for x in A.items:
        if B.contains(x): result.incl(x)

# Integer set intersection
proc `*`*(A: TIntSet, B: TIntSet) : TIntSet =
    result = initIntSet()
    for x in A.items:
        if B.contains(x): result.incl(x)

# Set union
proc `+`*[T](A: TSet[T], B: TSet[T]) : TSet[T] =
    result = initSet[T]()
    for x in A.items: result.incl(x)
    for x in B.items: result.incl(x)

# Jaccard coefficient
proc jaccard*[T](A: TSet[T], B:TSet[T]) : float = (A*B).len/(A+B).len

# Return an arbitrary element from a set (non-random)
proc someElement*[T](A: TSet[T]) : T =
    for x in A.items: return x

# Remove an arbitrary element (non-random)
proc removeSomeElement*[T](A: var TSet[T]) : T =
    if A.len == 0:
        raise newException(EInvalidIndex, "no more elements to remove")
    result = A.someElement
    A.excl(result)

proc choose2*(n:int):int = int(n*(n-1)/2)

# For a table with values that can be keys, create an inverted map
proc invert*[S,T](table:TTable[S,T]) : TTable[T,S] =
    result = initTable[T,S]()
    for key, val in table:
        result[val] = key
