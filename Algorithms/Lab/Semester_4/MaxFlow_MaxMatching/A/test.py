import random
import subprocess
import networkx as nx

def test():
    n = random.randint(3, 500)
    cn = random.randint(2, n-1)
    k = random.random()*0.9 + 0.05

    print("--------------------")    

    g=nx.connected_watts_strogatz_graph(n,cn,k).to_undirected()
     
    print(len(g.nodes()))
    print(len(g.edges()))
    attrs={}
    
    for e in g.edges():
        attrs[e] = random.random() * 10000
        #attrs[e] = random.randint(0, 100)/10
        print(e[0] + 1, e[1] + 1, attrs[e])
    
    nx.set_edge_attributes(g, 'capacity', attrs)
    
    flow = nx.maximum_flow_value(g, 0, n-1)
    
    print(flow)

    res = 1000

    with subprocess.Popen(["./main"], text=True, stdin=subprocess.PIPE, stdout=subprocess.PIPE) as proc:
        print(len(g.nodes()), len(g.edges()), file=proc.stdin)
        
        for e in g.edges():
            print(e[0] + 1, e[1] + 1, attrs[e], file=proc.stdin)
        proc.stdin.close()
        
        out = proc.stdout.readlines()
        #print(out)
        
        s = 0
        outF = list(map(float, out))
        for x in zip(g.edges(), outF[1:]):
            source = x[0][0]
            target = x[0][1]
            weight = x[1]

            if source == 0:
                s += weight
            if target == 0:
                s -= weight
        
        x=float(out[0])
        print("Flow:",flow," Sum:", s, " X:", x) 
        res = abs(flow - x) + abs(s - flow)
    return res

while test() < 0.0001:
    pass 
