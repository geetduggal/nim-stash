# Basic statistics
import math, algorithm, strutils
import matrix, gutils, random

# VECTOR GENERATION

proc irange*(a:int, b:int, step=1):seq[int] =
    ## Returns a sequence of integers using the countup iterator
    result = @[]
    for i in countup(a,b,step):
        result.add(i)

proc linspace*(a: float, b: float, n = 50) : seq[float] =
    result = @[]
    if b < a: raise newException(EInvalidIndex, "a must be < b")
    let step = (b-a)/(n-1).toFloat
    var x = a
    for i in 1 .. n:
        result.add(x)
        x += step

# EXTRA RANDOM FUNCTIONS

proc shuffle*[T](x: var seq[T], rng: var TRanGen) =
    let n = x.len
    for i in (n-1).countdown(1):
        var j = rng.sample(i+1)
        swap(x[i], x[j])

# PERCENTILES AND SUMMARIES

proc percentiles*[T](x:seq[T], percents:seq[int], reversed = false) : seq[T] =
    ## Returns the value of the ith percentile in x for each i in percents
    var sx = x
    proc gcmp[T](x, y: T): int = cmp[T](x,y)
    if reversed: sx.sort(gcmp[T], TSortOrder.Descending)
    else: sx.sort(gcmp[T])
    proc pidx(i:int):T =
        var idx = (i.toFloat/100.0*sx.len.toFloat-1).floor.toInt
        if idx < 0: idx = 0
        result = sx[idx]
    result = @[]
    for i in percents: result.add(pidx(i))

type TSummary*[T] = object
    percents* : seq[int]
    values* : seq[T]

proc filterNonZero*[T](x: openarray[T]) : seq[T] =
    result = @[]
    for i in x.items:
        if i > 0: result.add(i)

proc summarize*[T](x:seq[T], n=10, nonzero=true) : TSummary[T] =
    ## Returns a summary of the data set given n percentiles
    ## By default, only non-zero values are considered
    var xf = x
    if nonzero: xf = x.filterNonZero()
    result.percents = irange(0,100, int(100/n))
    result.values = percentiles(xf,result.percents)

proc toString*[T](s: TSummary[T]) : string =
    ## String representation of a data summary
    result = ""
    for i in 0 .. s.percents.len-1:
        result.add($s.percents[i] & "\t" & $s.values[i] & "\n")

proc `$`*[T](s: TSummary[T]) : string = s.toString

proc numNonZero*[T](x : openarray[T]) : int =
    ## Returns the number of non-zero entries in a numeric array
    result = 0
    for i in x:
        if i > 0: result += 1

# HISTOGRAMS

type THistogram*[T] = object
    bins: seq[T]
    counts: seq[int]
    totalCount: int

proc hidx(xi:float, binWidth:float, min:float) : int =
    max(0,ceil(round((xi-min)/binWidth).toFloat).toInt - 1)

proc histogram*[T](x:openarray[T], numBins:int, min:T, max:T) : THistogram[T] =
    result.bins = linspace(min, max, numBins+1)
    result.totalCount = x.len
    let binWidth = result.bins[1]-result.bins[0]

    newSeq(result.counts, numBins)
    for xi in x:
        result.counts[hidx(xi,binWidth,min)] += 1

proc `$`*[T](hist:THistogram[T]) : string =
    result = ""
    for i in 0 .. <hist.counts.len:
        if i == 0: result &= "["
        else: result &= "("
        result &= formatFloat(hist.bins[i],precision=3) & "," &
                  formatFloat(hist.bins[i+1],precision=3) & "]: " &
                  $hist.counts[i]
        if i != hist.bins.len - 1: result &= "\n"

type THistogram2d*[T] = object
    bins: seq[T]
    counts: TMatrix[int]
    totalCount: int

proc histogram2d*[T](x:openarray[T], y:openarray[T], numBins:int, min:T, max:T) : THistogram2d[T] =
    if x.len != y.len:
        raise newException(EInvalidIndex, "len(x) must equal len(y)")
    result.bins = linspace(min, max, numBins+1)
    result.totalCount = x.len
    let binWidth = result.bins[1]-result.bins[0]

    result.counts = zeros[int](numBins, numBins)
    for i in 0 .. <x.len:
        let ii = hidx(x[i],binWidth,min)
        let jj = hidx(y[i],binWidth,min)
        let curVal = result.counts[ii,jj]
        result.counts[ii, jj] = curVal + 1

proc `$`*[T](hist2d:THistogram2d[T]) : string =
    result = "BINS:\n"
    result &= $(hist2d.bins) & "\n\n"
    result &= "COUNTS:\n"
    result &= $(hist2d.counts)

# PROBABILITY AND INFORMATION THEORY

template square*[T](x:T) : T = x*x

proc sdev*[T](x:openarray[T], xmean:float) : float =
    let n = x.len
    result = 0.0
    for i in 0 .. <n:
        result += square(x[i]-xmean)
    result *= 1/(n-1)
    result = sqrt(result)


proc pearson*[T](x:openarray[T], y:openarray[T]) : float =
    let n = x.len
    let xmean = x.mean()
    let xsdev = x.sdev(xmean)
    let ymean = y.mean()
    let ysdev = y.sdev(ymean)

    result = 0.0
    for i in 0 .. <n:
        result += ((x[i]-xmean)/xsdev)*((y[i]-ymean)/ysdev)
    result *= 1/(n-1)

proc norm*[T](s:openarray[T]) : float =
    result = 0.0
    for x in s: result += square(x)
    result = sqrt(result)

template p*[T](hist: THistogram[T], i: int) : float = hist.counts[i]/(hist.totalCount)
template p*[T](hist: THistogram2d[T], i: int, j:int) : float = hist.counts[i,j]/(hist.totalCount)

proc pxlogpx(px:float) : float {.inline.} =
    if px == 0: return 0
    return px*log2(px)

proc pxylogpxy(pxy, px, py: float) : float {.inline.} =
    if pxy == 0: return 0
    if px*py == 0: return inf
    return pxy*log2(pxy/(px*py))

proc H*[T](histx:THistogram[T]) : float =
    let n = histx.counts.len
    result = 0
    for i in 0 .. <n:
        result += pxlogpx(histx.p(i))
    result *= -1

proc H*[T](x:openarray[T], numBins:int) : float = H(histogram(x,numBins, x.min,x.max))

proc I*[T](histx, histy:THistogram[T], histxy:THistogram2d[T]) : float =
    let n = histx.counts.len

    result = 0
    for i in 0 .. <n:
        for j in 0 .. <n:
            result += pxylogpxy(histxy.p(i,j), histx.p(i), histy.p(j))

proc I*[T](x:openarray[T], y:openarray[T], numBins : int) : float =
    let minBoth = min(x.min, y.min)
    let maxBoth = max(x.max,y.max)
    I(histogram(x, numBins,minBoth,maxBoth),
      histogram(y, numBins,minBoth,maxBoth),
      histogram2d(x,y, numBins,minBoth,maxBoth))

proc VI*[T](x:openarray[T], y:openarray[T], numBins: int) : float =
    let minBoth = min(x.min, y.min)
    let maxBoth = max(x.max,y.max)
    let histx = histogram(x, numBins, minBoth, maxBoth)
    let histy = histogram(y, numBins, minBoth, maxBoth)
    let histxy = histogram2d(x,y, numBins, minBoth, maxBoth)
    return H(histx) + H(histy) -2*I(histx, histy, histxy)

proc G*[T](x:openarray[T], y:openarray[T], numBins: int) : float =
    return (x.norm+y.norm)*(1-VI(x,y,numBins)/log2(x.len.toFloat+1))
    # return (x.norm+y.norm)*I(x,y,numBins)

type TRunningVariance* = object
    n : int
    mean : float
    m2 : float

proc initRunningVariance*() : TRunningVariance =
    result.n = 0
    result.mean = 0.0
    result.m2 = 0.0

proc add*(rv : var TRunningVariance,  x: float) =
    rv.n += 1
    let delta = x - rv.mean
    rv.mean += delta/float(rv.n)
    rv.m2 += delta*(x-rv.mean)

proc remove*(rv : var TRunningVariance, x: float) =
    rv.n -= 1
    let delta = x - rv.mean
    rv.mean -= delta/float(rv.n)
    rv.m2 -= delta*(x-rv.mean)

proc update*(rv : var TRunningVariance, oldx, newx: float) =
    let delta = newx - oldx
    let dold = oldx - rv.mean
    rv.mean += delta/float(rv.n)
    let dnew = newx - rv.mean
    rv.m2 += delta*(dold + dnew)


proc getVariance*(rv : TRunningVariance) : float = rv.m2/float(rv.n-1)

proc sampvar*(samp : openarray[float]) : float =
    var rv = initRunningVariance()
    for x in samp: rv.add(x)
    return rv.getVariance

# BASIC TESTS

when isMainModule:
    var rng = initRanGen()
    let a = @[1.0, 2.0, 3.0]
    echo histogram(a, 10, a.min, a.max)
    var b = a
    b.shuffle(rng)
    echo histogram2d(a,b,10,a.min,a.max)
    echo linspace(0,1,10)
    echo linspace(0,1,11)
    let x = linspace(0,10,100)
    echo histogram(x, 10,x.min,x.max)
    echo histogram2d(x,x, 10,x.min,x.max)
    var y = x
    y.shuffle(rng)
    echo histogram2d(x,y,10,x.min,x.max)
    echo I(x,x,100)
    echo I(x,y,100)
    let z = zerovec[float](x.len, 0.04)
    echo I(x,z,100)
    var y2 = y
    y2[3] = 0
    y2[5] = 0
    echo I(x,y2,100)
    echo sum([0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1])

    proc testStat(x:openarray[float],y:openarray[float]) =
        echo "x = " & $x
        echo "y = " & $y
        vecho pearson(x,y)
        vecho I(x,y,10)
        vecho G(x,y,10)
        echo "---"


    testStat([1.0,2.0,3.0], [1.0,2.0,3.0])
    testStat([0.0],[0.0])
    testStat([1.0],[1.0])
    testStat([5.0],[5.0])
    testStat([10.0],[5.0])
    testStat([5.0,5.0,0.0,0.0], [0.0,0.0,5.0,5.0])
    testStat([0.0,1.0,2.0,3.0,4.0], [4.0,1.0,0.0,1.0,4.0])
