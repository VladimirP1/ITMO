#include <iostream>
#include <sstream>
#include <vector>
std::stringstream in;
std::vector<int> mapping;
std::vector<int> rev_mapping;
void transform() {
    int n, m, tmp1, tmp2;
    std::stringstream buf;
    std::cin >> n >> m;
    mapping.resize(n);
    rev_mapping.resize(n);

    for (int i = 0; i < m; i++) {
        std::cin >> tmp1 >> tmp2;
        buf << tmp1 << " " << tmp2 << std::endl;
    }
    for (int i = 0; i < n; i++) {
        std::cin >> tmp1;
        buf << tmp1 << " ";
        mapping[tmp1] = i;
        rev_mapping[i] = tmp1;
    }
    buf << std::endl;

    in << n << " " << m << std::endl;
    for(int i = 0; i < 2*m + n; i++) {
        buf >> tmp1;
        in << mapping[tmp1] << " ";
    }
}

struct Edge{
    Edge(int a, int b, int id);
    int a, b, id, used = 0;
    bool x(const Edge& b) const;
};

struct Graph{
    int& n() { return _n; }
    int m() { return _edges.size(); }
    std::vector<Edge>& edges() { return _edges; }
    void add_edge(int a, int b, int id);
    std::vector<Edge> _rej;
private:
    int _n;
    std::vector<Edge> _edges;
};

Graph g;

void read() {
    int t1, t2, t3;
    in >> g.n() >> t1;
    for(int i = 0; i < t1; i++) {
        in >> t2 >> t3;
        g.add_edge(t2, t3, i);
    }
}

void fail() {
    std::cout << "NO" << std::endl;
    exit(0);
}

void bipartiteDFS(int v, int color) {
    if(g.edges()[v].used){
        if(g.edges()[v].used != color) {
            fail();
        }
        return;
    }
    g.edges()[v].used = color;
    for(int i = 0; i < g.edges().size(); i++) {
        if(g.edges()[i].x(g.edges()[v]) || g.edges()[v].x(g.edges()[i])) {
            bipartiteDFS(i, !(color - 1) + 1);
        }
    }
}

void checkBipartite() {
    for(int i = 0; i < g.edges().size(); i++) {
        if(!g.edges()[i].used) {
            bipartiteDFS(i, 1);
        }
    }
}

void print() {
    std::cout << "YES" << std::endl;
    int m = g.n() + g.m();
    for(int i = 0; i < g.n(); i++) {
        std::cout << 2*(rev_mapping[i] - 1) << " " << 0 << " ";
    }
    std::cout << std::endl;
    auto it = g.edges().begin();
    auto itR = g._rej.begin();
    for(int i = 0; i < m; i++) {
        if(it != g.edges().end() && it->id == i) {
            std::cout << it->a + it->b << " " << (it->used == 2 ? "-" : "") << it->b - it->a << std::endl;
            ++it;
        } else if(itR != g._rej.end() && itR->id == i){
            std::cout << itR->a + itR->b << " " << itR->b - itR->a << std::endl;
            ++itR;
        }
    }
}

int main()
{
    transform();
    read();
    checkBipartite();
    print();

    return 0;
}

Edge::Edge(int a, int b, int id) : a(a), b(b), id(id)
{}

bool Edge::x(const Edge &e) const {
    if (id == e.id) return false;
    if (a == e.a || a == e.b || b == e.a || b == e.b) {
        return false;
    }
    if ((a < e.a && e.a < b) != (a < e.b && e.b < b)) {
        return true;
    }
    return false;
}

void Graph::add_edge(int a, int b, int id) {
    int f = std::min(a,b), t = std::max(a,b);
    if (t - f != 1 && !(f == 0 && t == (_n - 1))) {
        _edges.push_back(Edge(f, t, id));
    } else {
        _rej.push_back(Edge(f,t,id));
    }
}
