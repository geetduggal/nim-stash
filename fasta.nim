# Use fastentries to do simple FASTA file parsing
import fastentries

# Use '>' as a separator and iterate through fasta entries
iterator fastaEntries*(fname: string) : tuple[name,s:string] =
    for entryInfo in entriesMapped(fname, sep=int('>')):
        if entryInfo.numChars == 0: continue
        # Jump to the newline to get at the actual sequence
        let seqInfo = entryInfo.skipToChar(10)

        # Build the strings (converting to uppercase by default)
        let s = seqInfo.toGCString(skipNewline=true, upper=true)
        var nameInfo : TEntry = (entryInfo.start, seqInfo.start - entryInfo.start)
        let name = nameInfo.toGCString
        yield (name, s)

