#include <iostream>
#include <algorithm>
#include <vector>
#include <array>
#include <set>

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
    int f() const {return _from;}
    int t() const {return _to;}
    _dtype w() const {return _weight;}
    bool operator<(const Edge& arg) const {
        if(_from < arg._from) return true;
        if((_from == arg._from) && (_to < arg._to)) return true;
        return false;
    }
};

const int N = 2000, M = 5000;
int n, m, s;
std::vector<Edge> edges;

void read() {
    int a, b;
    _dtype w;
    std::cin >> n >> m >> s;
    --s;

    for(int i = 0; i < m; i++) {
        std::cin >> a >> b >> w;
        auto ins = Edge(a - 1, b - 1, w);
        edges.push_back(ins);
    }
}

std::array<_dtype, N> D;


void initFordBellman() {
    std::fill(D.begin(), D.end(), std::numeric_limits<_dtype>::max());
    D[s] = 0;
}

void fordBellman() {
    for(int i = 0; i < n; i++) {
        for(auto e : edges) {
            if(D[e.f()] != std::numeric_limits<_dtype>::max())
                D[e.t()] = std::min(D[e.f()] + e.w(), D[e.t()]);
        }
    }
}

void write() {
    for(int i = 0; i < n; i++) {
        auto z = D[i];
        if (z == std::numeric_limits<_dtype>::max()) z = -1;
        std::cout << z << std::endl;
    }
}

int main()
{
    read();
    initFordBellman();
    fordBellman();
    auto oldD = D;
    fordBellman();
    std::vector<std::pair<int, _dtype>> result;
    enum {INFO_OK = 0, INFO_NOPATH = 2, INFO_NOTDEFINED = 3};
    result.resize(n);
    for(int i = 0; i < n; i++) {
        if(D[i] != oldD[i]) {
            result[i].first = INFO_NOTDEFINED;
        } else if(oldD[i] == std::numeric_limits<_dtype>::max()) {
            result[i].first = INFO_NOPATH;
        } else {
            result[i].first = INFO_OK;
            result[i].second = oldD[i];
        }
    }
    for(int i = 0; i < n; i++) {
        for(auto e : edges) {
            if(result[e.f()].first == INFO_NOTDEFINED) {
                result[e.t()].first = INFO_NOTDEFINED;
            }
        }
    }
    for(auto i : result) {
        if(i.first == INFO_OK) {
            std::cout << i.second << std::endl;
        } else if(i.first == INFO_NOPATH) {
            std::cout << "*" << std::endl;
        } else if(i.first == INFO_NOTDEFINED) {
            std::cout << "-" << std::endl;
        }
    }
    //write();

    return 0;
}
