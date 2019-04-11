#include <iostream>
#include <vector>
#include <numeric>
#include <algorithm>
#include <set>

class DSU {
public:
    DSU(int size) {
        k.resize(size);
        std::iota(k.begin(), k.end(), 0);
    }
    int up(int v) {
        // TBD: path compression
        while (k[v] != v) {
            v = k[v];
        }
        return v;
    }
    void merge(int v, int u) {
        int tv = up(v);
        int tu = up(u);
        if (rand() % 2) std::swap(tu, tv);
        k[tu] = tv;
    }
private:
    std::vector<int> k;
};

struct Edge {
    Edge(int f, int t, uint64_t w, int id) : f(f), t(t), w(w), id(id) {}
    int f, t;
    uint64_t w;
    int id;
    bool inTree = false;
    bool operator<(const Edge& b) const {
        return w < b.w;
    }
};

int n, m;
uint64_t s, total;

std::vector<Edge> edges;

void read() {
    total = 0;
    int t1, t2;
    uint64_t t3;
    std::cin >> n >> m >> s;
    for(int i = 0; i < m; i++) {
        std::cin >> t1 >> t2 >> t3;
        edges.push_back({t1 - 1, t2 - 1, t3, i});
        total += t3;
    }
    sort(edges.begin(), edges.end());
}

uint64_t kruskal() {
    uint64_t sum = 0;
    DSU d(n);
    auto iter = edges.rbegin();
    while (iter != edges.rend()) {
        if(d.up(iter->f) != d.up(iter->t)) {
            const_cast<Edge&>(*iter).inTree = true;
            d.merge(iter->f, iter->t);
            sum += iter->w;
        }
        ++iter;
    }
    return sum;
}

int main()
{
    std::freopen("destroy.in", "r", stdin);
    std::freopen("destroy.out", "w", stdout);
    std::ios_base::sync_with_stdio(false);

    read();
    uint64_t sum = kruskal();
    auto it = edges.rbegin();
    while((total > sum + s) && it != edges.rend()) {
        if (!it->inTree) {
            const_cast<Edge&>(*it).inTree = true;
            sum += it->w;
        }
        ++it;
    }
    std::vector<int> notUsed;
    for(auto e : edges) {
        if (!e.inTree) {
            notUsed.push_back(e.id);
        }
    }
    std::sort(notUsed.begin(), notUsed.end());
    std::cout << notUsed.size() << "\n";
    for(auto i : notUsed) {
        std::cout << i + 1 << " ";
    }
    std::cout << "\n";



    return 0;
}
