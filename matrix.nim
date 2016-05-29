# Simplistic matrix module partially inspired from:
# http://stackoverflow.com/questions/21416013/how-do-you-use-matrices-in-nimrod
import strutils, gutils, math, random, algorithm

type TMatrix*[T] = object
    nrows*, ncols*: int
    data*: seq[T]

proc index*[T](A: TMatrix[T], r,c: int): int {.inline.} =
    if r<0 or r>A.nrows-1 or c<0 or c>A.ncols-1:
        raise newException(EInvalidIndex, "matrix index out of range")
    result = r*A.ncols+c

proc alloc*[T](A: var TMatrix[T], nrows,ncols: int) {.inline.} =
    ## Allocate space for a m x n matrix
    A.nrows = nrows
    A.ncols = ncols
    newSeq(A.data, nrows*ncols)

proc zeros*[T](nrows,ncols: int, val:T=0): TMatrix[T] =
    ## Construct a m X n matrix filled with 0s
    result.alloc(nrows, ncols)
    for i in 0 .. <nrows:
        for j in 0 .. <ncols:
            result[i,j] = val

proc create*[T](nrows,ncols: int, x: openarray[T]): TMatrix[T] =
    ## Construct a m X n matrix given a data array
    result.alloc(nrows, ncols)
    let n = nrows*ncols
    if x.len > 0:
        if x.len < n:
            raise newException(EInvalidIndex, "insufficient data provided")

        for i in 0 .. n-1:
            result.data[i] = x[i]

proc `[]`*[T](A: TMatrix[T], r,c: int): T =
    ## Return the element at A[r,c]
    result = A.data[A.index(r,c)]

proc `[]=`*[T](A: var TMatrix[T], r,c: int, val: T) =
    ## Sets A[r,c] to val
    A.data[A.index(r,c)] = val

proc boundsFromFile*(fname: string) : tuple[nrows, ncols: int] =
    # Obtain the bounds of a matrix from a file
    var ncols : int
    for line in lines(fname):
        ncols = line.split.len
        break

    var nrows = 0
    for line in lines(fname): nrows += 1
    result = (nrows, ncols)

proc fromFile*(fname: string, nrows, ncols: int): TMatrix[float] =
    # Read matrix of floating point # from a file given bounds
    result.alloc(nrows, ncols)

    for i,line in pairs(Open(fname)):
        let fields = line.split.map(proc(x:string):float = x.ParseFloat)
        for j,x in pairs(fields):
            result[i,j] = x
            if j == ncols-1: break
        if i == nrows-1: break

proc fromFile*(fname: string): TMatrix[float] =
    ## Read a matrix of floating point #s from a file
    var (nrows, ncols) = boundsFromFile(fname)
    result = fromFile(fname, nrows, ncols)

iterator elements*[T](A: TMatrix[T]): tuple[i:int, j:int, x:T] =
    ## Iterates through matrix elements row-wise
    for i in 0 .. A.nrows-1:
        for j in 0 .. A.ncols-1:
            yield (i,j,A[i,j])

proc toString*[T](A: TMatrix[T]) : string =
    ## String representation of matrix
    result = ""
    for i in 0 .. A.nrows-1:
        for j in 0 .. A.ncols-1:
            result.add(float(A[i,j]).formatFloat(precision=3) & "\t")
        result.add("\n")

proc `$`*[T](A: TMatrix[T]) : string = toString(A)


proc copy*[T](A: TMatrix[T]) : TMatrix[T] =
    result.alloc(A.nrows, A.ncols)
    for i,j,x in A.elements:
        result[i,j] = x


proc removeNodes*[T](A: var TMatrix[T], ids: seq[int]) =
    ## Zero out the row and columns for ids
    ## The matrix is assumed to be square
    if A.nrows != A.ncols:
        raise newException(EInvalidIndex, "matrix not square")
    for i in ids:
        for j in 0 .. A.nrows-1:
            A[i,j] = 0
            A[j,i] = 0

proc allZeros*[T](A: TMatrix[T], row:int) : bool =
    for i in 0 .. A.ncols-1:
        if A[row,i] != 0: return false
    return true

proc numConnectedNodes*[T](A: TMatrix[T]) : int =
    result = 0
    for i in 0 .. A.nrows-1:
        if not A.allZeros(i): result += 1

proc row*[T](A: TMatrix[T], id : int) : seq[T] =
    result = @[]
    for i in 0 .. <A.ncols:
        result.add(A[id,i])

proc col*[T](A: TMatrix[T], id : int) : seq[T] =
    result = @[]
    for i in 0 .. <A.nrows:
        result.add(A[i,id])
