#include <vector>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <map>
#include <sstream>
#include <set>
#include <assert.h>
#include <math.h>

const int precision = 6;

void dec() {}

template <typename T, typename... Args>
void dec(T& a, Args&... args) {
    --a;
    dec(args...);
}

struct Edge {
    int from, to;
    double capacity;
    double flow = 0;
    bool isReverse;
    Edge* complement;

    static void mkEdges(int a, int b, double capacity, Edge*& direct, Edge*& reverse) {
        direct = new Edge(a,b,capacity, false);
        reverse = new Edge(b,a,capacity, true);
        direct->complement = reverse;
        reverse->complement = direct;
    }

    void addFlow(double dFlow) {
        capacity -= dFlow;
        complement->capacity += dFlow;
        flow += dFlow;
        complement->flow -= dFlow;
    }

    Edge() {} // for std::vector
private:
    Edge (int from, int to, double capacity, bool isReverse) : from(from), to(to), capacity(capacity), isReverse(isReverse) {}
};

struct Vertex {
    std::vector<Edge*> edges;
    Edge* parent = 0;
};

struct Graph {
    int N, M;
    std::vector<Vertex> verticies;

    void read(std::istream& in, std::vector<Edge*> &outputOrder) {
        in >> N >> M;

        verticies.resize(N);

        for(int i = 0, a ,b; i < M; i++) {
            double c;
            in >> a >> b >> c;
            dec(a,b);

            Edge *direct, *reverse;
            Edge::mkEdges(a, b, c, direct, reverse);

            verticies[a].edges.push_back(direct);
            verticies[b].edges.push_back(reverse);

            outputOrder.push_back(direct);
        }
    }
};

double fordFulkBfs(Graph& g) {
    for(int i = 0; i < g.verticies.size(); i++) {
        g.verticies[i].parent = 0;
    }
    g.verticies[0].parent = (Edge*)1;

    std::set<std::pair<int, double>> q{{0,std::numeric_limits<double>::max()}};

    while (!q.empty()) {
        int v;
        double minCap;
        std::tie(v, minCap) = *q.begin();
        q.erase(q.begin());

        if(v == (g.verticies.size() - 1)) {
            Edge* e;
            //std::cout << "Pushing " << minCap << " of flow through ..." << std::endl;
            while ((e = g.verticies[v].parent) > (Edge*)1) {
                //std::cout << "\t" << e->from + 1 << " <-> " << e->to + 1 << std::endl;
                v = e->from;
                e->addFlow(minCap);
            }
            return minCap;
        }

        for(auto& e : g.verticies[v].edges) {
            //std::cerr << "--- " << e->capacity << std::endl;
            if (g.verticies[e->to].parent == 0 && g.verticies[e->to].parent != (void*)1 && e->capacity > 0.0000001) {
                g.verticies[e->to].parent = e;
                assert(e->from == v);
                q.insert({e->to, std::min(minCap, e->capacity)});
            }
        }
    }
    return 0;
}

void solve(std::istream& in, std::ostream& out) {
    out.precision(precision);
    out << std::fixed;

    Graph g;
    std::vector<Edge*> outOrder;
    g.read(in, outOrder);

    while(fordFulkBfs(g) > 0.000001);

    double totalFlow = 0;

    for(auto& e : g.verticies[0].edges) {
        totalFlow += e->flow;
    }

    out << totalFlow << std::endl;

    for(auto& e: outOrder) {
        out << e->flow * (e->isReverse? -1 : 1) << std::endl;
    }
}

int main() {
    //read();
    solve(std::cin, std::cout);

    return 0;
}
