import graphviz
import re
import math
import itertools

def parseNode(l):
    n = l[6:8]
    rate = int(re.search("rate=(.*);",l).group(1))
    conns = re.search("to valve[s]? (.*)$", l).group(1).split(", ")
    return (n,rate,conns)

def rawGraph(contents):
    return list(map(parseNode, contents.splitlines()))

def convertGraph(rawNodes):
    nodes = [(n,r) for n,r,_ in rawNodes]
    edges = [(n,c,1) for n,_,cs in rawNodes for c in cs  if c>n]
    return nodes,edges

def getEdges(n,es):
    return [(a,b,w) for a,b,w in es if a==n or b==n]

def pruneGraph(ns,es):
    ns = ns.copy()
    es = es.copy()

    delns = [(n,r) for n,r in ns if r==0 and len(getEdges(n,es))==2]

    for n,r in delns:
        ns.remove((n,r))

        e1,e2 = getEdges(n,es)
        es.remove(e1)
        es.remove(e2)
        a = e1[0] if e1[1] == n else e1[1]
        b = e2[0] if e2[1] == n else e2[1]
        w = e1[2]+e2[2]
        es.append((a,b,w))

    return ns,es

def allDistances(nodes,edges):
    maxDist = sum([w for a,b,w in edges])
    snodes = sorted(n for n,_ in nodes)
    dists = {(ni,nj):maxDist for i,ni in enumerate(snodes) for nj in snodes[i:]}

    for a,b,w in edges:
        if a<b:
            dists[(a,b)] = w
        else:
            dists[(b,a)] = w
    for n,_ in nodes:
        dists[(n,n)] = 0

    for nk,_ in nodes:
        for ni,_ in nodes:
            pik = (ni,nk) if ni<nk else (nk,ni)
            for nj,_ in nodes:
                pij = (ni,nj) if ni<nj else (nj,ni)
                pkj = (nk,nj) if nk<nj else (nj,nk)
                newd = dists[pik] + dists[pkj]
                if newd < dists[pij]:
                    dists[pij] = newd

    return dists

def fullGraph(nodes,edges):
    dists = allDistances(nodes,edges)
    tedges = []
    for i,nni in enumerate(nodes):
        ni,_ = nni
        for nj,_ in nodes[i+1:]:
            pij = (ni,nj) if ni<nj else (nj,ni)
            tedges.append((ni,nj,dists[pij]))
    return tedges

def showGraph(nodes,edges):
    dot = graphviz.Graph()
    for n,r in nodes:
        dot.node(n, label=f"{n} ({r})")

    for a,b,w in edges:
        dot.edge(a,b,label=str(w))

    dot.render(view=True)

def pressureReleased(sequence, nodes, dists):
    p = 0
    timeLeft = 30

    for i in range(1,len(sequence)):
        n = sequence[i]
        nm1 = sequence[i-1]
        comb = (n,nm1) if n<nm1 else (nm1,n)
        timeLeft -= dists[comb]+1
        if timeLeft <= 0:
            break
        else:
            p += timeLeft*[r for nn,r in nodes if nn==n][0]
    return p

def optimize(current, tLeft, toVisit, rates, dists):

    pmax = 0

    for nxt in toVisit:

        comb = (current,nxt) if current<nxt else (nxt,current)
        tLeftLocal = tLeft - dists[comb] - 1
        if tLeftLocal <= 0:
            # time is up, so don't bother searching more.
            continue

        p = tLeftLocal*rates[nxt] + optimize(nxt, tLeftLocal, [tv for tv in toVisit if tv != nxt], rates, dists)

        if p > pmax:
            pmax = p

    return pmax

contents = open(0).read()
nodes, edges = convertGraph(rawGraph(contents))

nodes,edges = pruneGraph(nodes,edges)

# tedges = fullGraph(nodes,edges)
dists = allDistances(nodes, edges)

toVisit = [n for n,r in nodes[1:]]

# seq = ['AA']+ sorted(toVisit)
# seq = ['AA', 'DD', 'BB', 'JJ', 'HH', 'EE', 'CC']
nperms = math.factorial(len(nodes)-1)

rates = {n:r for n,r in nodes}
pmax = optimize('AA', 30, toVisit, rates, dists)

print("Max pressure:", pmax)
# i = 0
# pmax = 0
# for perm in itertools.permutations(toVisit):

#     pcnt = 100* i/float(nperms)
#     if i % 100000 == 0:
#         print(f"permutation {i} / {nperms}  {pcnt:8.5f} %")

#     seq = ['AA']+ list(perm)
#     p = pressureReleased(seq, nodes, dists)
#     if p > pmax:
#         pmax = p
#         print("pressure for", ">".join(seq)," = ", p)
#     i += 1

# showGraph(nodes,edges)
# print("edges total graph:", len(tedges))

print(f"combinations = {len(nodes)-1}! =",nperms)
print("nodes:", len(nodes))
print("edges:", len(edges))
