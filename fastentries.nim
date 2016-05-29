# Reads entries in a file fast using mmap
import memfiles, strutils

# Finds the first character match using memchr (super fast)
proc firstChar*(s: pointer; c,n: int) : pointer {.importc: "memchr",
    header: "<string.h>"}

type TEntry* = tuple[start: TAddress, numChars: int]

# Skip to a character (not used anymore)
proc skipToChar*(e:TEntry, c: int): TEntry =
    let caddr = cast[TAddress](firstChar(cast[pointer](e.start), c, e.numChars))
    if caddr > 0:
        return (caddr, e.numChars-(caddr-e.start))
    else:
        return (0,0)

# Convert a raw C-like string to a Garbage collected one
proc toGCString*(entryInfo : TEntry, skipNewline=false, upper=false) : string =
    if entryInfo.numChars == 0: return ""
    result = newString(entryInfo.numChars)
    var idx = 0
    for i in entryInfo.start .. <(entryInfo.start+entryInfo.numChars):
        var ch = cast[ptr char](i)[]
        if skipNewLine and int(ch) == 10: continue
        if upper: ch = ch.toUpper
        result[idx] = ch
        idx += 1
    result.setLen(idx)

# An iterator to go through entries separated by a particular character
# (default is newline)
iterator entriesMapped*(fname: string, sep=10) : TEntry  =
    var f = open(fname, mappedSize = -1)
    var floc = cast[TAddress](f.mem)
    let fend = cast[TAddress](f.mem) + f.size
    var bytesLeft = f.size
    var newLineLoc : TAddress = 1
    while newLineLoc != 0:
        newLineLoc = cast[TAddress](cast[pointer](floc).firstChar(sep,bytesLeft))
        if floc != fend:
            # Thanks to Araq for the suggestion
            yield (floc, (if newLineLoc == 0: fend else: newLineLoc) - floc)
        if newLineLoc == 0: break
        floc = newLineLoc + 1
        bytesLeft = fend - newLineLoc
    f.close

# Wrapper to iterate through lines and return a GC'ed Nimrod string
iterator fastlines*(fname: string) : string =
    for e in entriesMapped(fname):
        yield e.toGCString
