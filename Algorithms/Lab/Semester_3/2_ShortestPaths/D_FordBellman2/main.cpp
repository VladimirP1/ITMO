#include <iostream>
#include <algorithm>
#include <vector>
#include <array>

using _dtype = long long;
using _dvtype = std::pair<_dtype, int>;

class Edge{
    int _from, _to;
    _dtype _weight;
public:
    Edge(int _from, int _to, _dtype _weight):
        _from(_from),
        _to(_to),
        _weight(_weight) {};
    int f() {return _from;}
    int t() {return _to;}
    _dtype w() {return _weight;}
};

const int N = 10000, M = 10000;
int n, m, k, s;
std::vector<Edge> edges;
//std::vector<std::vector<_dvtype>> inc;

void read() {
    int a, b;
    _dtype w;
    std::cin >> n >> m >> k >> s;
    --s;

    for(int i = 0; i < m; i++) {
        std::cin >> a >> b >> w;
        //inc[b].push_back({w, a});
        edges.push_back(Edge(a - 1, b - 1, w));
    }
}

std::array<std::array<_dtype, N>, 2> D;

void swapBuffers() {swap(D[0], D[1]);}

void initFordBellman() {
    std::fill(D[0].begin(), D[0].end(), std::numeric_limits<_dtype>::max());
}

void fordBellman() {
    initFordBellman();
    D[0][s] = 0;
    for(int i = 0; i < k; i++) {
        swapBuffers();
        initFordBellman();
        for(auto e : edges) {
            if(D[1][e.f()] != std::numeric_limits<_dtype>::max())
                D[0][e.t()] = std::min(D[1][e.f()] + e.w(), D[0][e.t()]);
        }
    }
}

void write() {
    for(int i = 0; i < n; i++) {
        auto z = D[0][i];
        if (z == std::numeric_limits<_dtype>::max()) z = -1;
        std::cout << z << std::endl;
    }
}



int main()
{
    read();
    fordBellman();
    write();

    return 0;
}
