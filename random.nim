# Standard Merssene twister random number generation
# Used pseudocode from Wikipedia and a standard C++ implementation as
# a reference

type TRanGen* = object
    MT : array[0..623, int64]
    index : int64

proc initRanGen*(seed: int64 = 314159) : TRanGen =
    result.index = 0
    result.MT[0] = seed
    for i in 1 .. 623:
        result.MT[i] = (1812433253 * (result.MT[i-1] xor (result.MT[i-1] shr 30) + i)) and 0xffffffff

proc generateNumbers(rng : var TRanGen) =
    for i in 0 .. 623:
        let y = (rng.MT[i] and 0x80000000) + (rng.MT[(i+1) mod 624] and 0x7fffffff)
        rng.MT[i] = rng.MT[(i+397) mod 624] xor (y shr 1)
        if y mod 2 != 0: rng.MT[i] = rng.MT[i] xor 2567483615

proc sample*(rng : var TRanGen) : int64 =
    if rng.index == 0: rng.generateNumbers
    var y = rng.MT[rng.index]
    y = y xor (y shr 11)
    y = y xor ((y shl 7) and 2636928640)
    y = y xor ((y shl 15) and 4022730752)
    y = y xor (y shr 18)
    rng.index = (rng.index+1) mod 624
    return y

proc sample*(rng: var TRanGen, max: int) : int = int(rng.sample()) mod max

when isMainModule:
    var rng = initRanGen(1)
    for i in 0 .. 100:
        echo rng.sample(10)
