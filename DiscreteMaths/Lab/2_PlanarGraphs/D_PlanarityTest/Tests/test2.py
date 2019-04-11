import networkx as nx
import planarity
import random
import sys

N = 1000

def gen_graph():
	n = random.randint(2, 6)
	apm = random.randint(1, n*n)
	
	G = nx.Graph()
	
	for i in range(n):
		G.add_node(i + 1)
	
	for i in range(apm):
	    a = random.randint(1, n - 1)
	    b = random.randint(a + 1, n)
	    G.add_edge(a, b)
	
	M = nx.adjacency_matrix(G).toarray()
	
	for i in range(0, len(M)):
	    for j in range(0, i):
	        print(M[i][j], end='')
	
	print()
	
	GG = planarity.kuratowski_subgraph(G)
	
	print(len(GG.nodes()),file=sys.stderr)
	
	return "YES\n" if planarity.is_planar(G) else "NO\n"
	
print(N)
f = open('.is_planar', 'w')
for i in range(N):
    f.write(gen_graph())

f.close()
