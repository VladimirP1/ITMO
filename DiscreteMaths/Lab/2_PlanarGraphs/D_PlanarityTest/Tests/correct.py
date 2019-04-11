import networkx as nx
import planarity
import random
import sys

N = int(input())

def read_one(data):
	n = 1
	while len(data) > n*(n-1)//2:
		n += 1
		
	G = nx.Graph()
	
	for i in range(n):
		G.add_node(i)
		
	k = 0
	for i in range(n):
		for j in range(i):
			if (data[k] == '1'):
				G.add_edge(i, j)
			k += 1
	#print(k)
	
	print("YES" if planarity.is_planar(G) else "NO")
	
for i in range(N):
	read_one(input())
