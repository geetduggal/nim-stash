# Simplistic graph module to interface with matrices
# Depends on matrix just for conversion purposes

import matrix, tables, queues, sets, intsets
from sequtils import filter
import gutils

# Basic storage (adjacency list of integers using IntSets as the list
type TGraph* = object
    adjList* : TTable[int, TIntSet]

# Basic operations
proc copy*(G: TGraph) : TGraph =
    result.adjList = G.adjList

proc initGraph* : TGraph =
    result.adjList = initTable[int, TIntSet]()

proc addNode*(G: var TGraph, v:int) =
    if not G.adjList.hasKey(v):
        G.adjList[v] = initIntSet()

proc addEdge*(G: var TGraph, v:int, w:int) =
    G.addNode(v)
    G.addNode(w)
    G.adjList.mget(v).incl(w)
    G.adjList.mget(w).incl(v)

# Convert from a matrix ignoring self-loops and at a particular cutoff
proc fromMatrix*[T](A : TMatrix[T], cutoff:T=0) : TGraph =
    result = initGraph()
    ## Constructs a graph from an adjacency matrix
    for i,j,x in A.elements:
        if x > cutoff and i != j:
            result.addEdge(i,j)

proc numNodes*(G: TGraph) : int = G.adjList.len

# Iterate through the nodes, edges, and node-neighbors of a graph
iterator nodes*(G: TGraph) : int =
    for v in G.adjList.keys: yield v

type TEdge* = tuple[v:int,  w:int]
iterator edges*(G: TGraph) : TEdge =
    for v,neighbors in G.adjList.pairs:
        for w in neighbors:
            if v < w: yield (v,w)


iterator neighbors*(G: TGraph, v: int) : int =
    for w in G.adjList[v]: yield w

proc numNeighbors*(G: TGraph, v: int) : int =
    result = 0
    for w in G.neighbors(v): result += 1

# Removal operations
proc removeNode*(G: var TGraph, v: int) =
    for w in G.neighbors(v):
        G.adjList.mget(w).excl(v)
    G.adjList.del(v)

proc removeEdge*(G: var TGraph, v,w : int) =
    G.adjList.mget(v).excl(w)
    G.adjList.mget(w).excl(v)

proc numEdges*(G: TGraph) : int =
    result = 0
    for v,w in G.edges: result += 1

proc allone*(i,j:int): int {.procvar.} = 1

# Convert a graph to a matrix with a closure to determine
# edge weights (default is 1)
proc toMatrix*(G: TGraph,
    weight : proc (i,j:int): int {.closure.} = allone) : TMatrix[int] =
    let n = G.numNodes
    var nodeMap = initTable[int,int]()
    var i = 0
    for v in G.nodes:
        nodeMap[v] = i
        i+=1
    result = matrix.zeros[int](n,n)
    for v,w in G.edges:
        result[nodeMap[v],nodeMap[w]] = weight(v,w)
        result[nodeMap[w],nodeMap[v]] = weight(w,v)

# String representation of a graph
proc `$`*(G : TGraph) : string =
    result = ""
    for i,j in G.edges:
        result &= $i & " " & $j & "\n"

# Breadth-first traversal
iterator BFS*(G: TGraph, v:int) : int =
    var Q = initQueue[int]()
    var Vs = initSet[int]()
    Q.enqueue(v)
    Vs.incl(v)
    while Q.len > 0:
        let t = Q.dequeue
        yield t
        for u in G.neighbors(t):
            if not Vs.contains(u):
                Vs.incl(u)
                Q.enqueue(u)

proc hasNode*(G: TGraph, v: int) : bool = G.adjList.hasKey(v)

# Return the graph that is a subgraph given an array of nodes
proc subgraph*(G: TGraph, nodes: openarray[int]) : TGraph =
    result = initGraph()
    for v in nodes: result.addNode(v)
    for v,w in G.edges:
        if result.hasNode(v) and result.hasNode(w):
            result.addEdge(v,w)

# Intersect two graphs by returning a new graph with the intersected 
# nodes and associated adjacencies
proc `*`*(G: TGraph, H:TGraph) : TGraph =
    result = initGraph()
    for v in G.nodes:
        if H.adjList.hasKey(v):
            result.adjList[v] = (G.adjList[v] * H.adjList[v])

# Loop through connected components by their node lists
iterator componentNodes*(G: TGraph) : seq[int] =
    var remaining = initSet[int]()
    for v in G.nodes: remaining.incl(v)

    while remaining.len > 0:
        var component : seq[int] = @[]
        for v in BFS(G, remaining.someElement):
            component.add(v)
        for v in component: remaining.excl(v)
        yield component

# Loop through connected components as graph objects
iterator components*(G: TGraph) : TGraph =
    for component in G.componentNodes:
        yield G.subgraph(component)

proc hasEdge*(G: TGraph, v: int, w: int) : bool = G.adjList[v].contains(w)

proc isUndirected*(G: TGraph) : bool =
    for v,w in G.edges:
        if not (G.hasEdge(v,w) and G.hasEdge(w,v)): return false
    return true

# TODO: Modify based on new intset impl (simplifies)
# Based on: http://networkx.github.io/documentation/latest/_modules/networkx/algorithms/clique.html#find_cliques
iterator maxCliques*(G: TGraph) : seq[int] =
    var maxConn = -1
    var nnbrs = initTable[int, TSet[int]]()
    var pivotNbrs = initSet[int]()
    var pivotDoneNbrs : TSet[int]
    for v in G.nodes:
        nnbrs[v] = initSet[int]()
        for w in G.neighbors(v):
            if v != w: nnbrs.mget(v).incl(w)
        let conn = nnbrs[v].len
        if conn > maxConn:
            pivotNbrs = nnbrs[v]
            maxConn = conn
    var cand = initSet[int]()
    for v in G.nodes: cand.incl(v)
    var done = initSet[int]()
    var stack : seq[tuple[cand, done, smallCand : TSet[int]]] = @[]
    var smallCand = cand - pivotNbrs
    var cliqueSoFar : seq[int] = @[]
    while smallCand.len > 0 or stack.len > 0:
        var n : int
        try: n = smallCand.removeSomeElement()
        except EInvalidIndex:
            let s = stack.pop()
            cand = s.cand
            done = s.done
            smallCand = s.smallCand
            discard cliqueSoFar.pop()
            continue

        cliqueSoFar.add(n)
        cand.excl(n)
        done.incl(n)
        let nn = nnbrs[n]
        let newCand = cand * nn
        let newDone = done * nn
        if newCand.len == 0:
            if newDone.len == 0:
                let clique = cliqueSoFar
                yield clique
            discard cliqueSoFar.pop()
            continue
        if newDone.len == 0 and newCand.len == 1:
            var clique = cliqueSoFar
            clique.add(newCand.someElement)
            yield clique
            discard cliqueSoFar.pop()
            continue
        var numbCand = newCand.len
        var maxConnDone = -1
        for n in newDone.items:
            let cn = newCand * nnbrs[n]
            let conn = cn.len
            if conn > maxConnDone:
                pivotDoneNbrs = cn
                maxConnDone = conn
            if maxConnDone == numbCand: break
        if maxConnDone == numbCand:
            discard cliqueSoFar.pop()
            continue
        maxConn = -1
        for n in newCand.items:
            let cn = newCand * nnbrs[n]
            let conn = cn.len
            if conn > maxConn:
                pivotNbrs = cn
                maxConn = conn
                if maxConn == numbCand-1: break
        if maxConnDone > maxConn:
            pivotNbrs = pivotDoneNbrs
        stack.add((cand, done, smallCand))
        cand = newCand
        done = newDone
        smallCand = cand - pivotNbrs


when isMainModule:
    let A = matrix.create(6,6, [1,1,1,0,0,0,
                                1,1,1,0,0,0,
                                1,1,1,0,0,0,
                                0,0,0,1,1,1,
                                0,0,0,1,1,1,
                                0,0,0,1,1,1])
    echo A
    let G = fromMatrix(A)
    echo G

    echo "Testing BFS"
    for x in BFS(G, 0):
        echo x

    echo "Testing connected components"
    for component in G.componentNodes:
        for x in component:
            echo x
        echo "----"

    echo "Testing to matrix"

    echo G.toMatrix

    echo G.toMatrix(weight=proc(i,j:int): int = i*j)

    echo G.subgraph([0,1,2])

    echo "Testing maximum cliques"
    var H = initGraph()
    H.addEdge(0,1)
    H.addEdge(1,2)
    H.addEdge(0,2)
    H.addEdge(2,3)
    H.addEdge(2,7)
    H.addEdge(3,7)
    H.addEdge(3,6)
    H.addEdge(6,5)
    H.addEdge(5,4)
    H.addEdge(3,4)
    H.addEdge(4,6)
    H.addEdge(3,5)

    for clique in H.maxCliques:
        echo clique
