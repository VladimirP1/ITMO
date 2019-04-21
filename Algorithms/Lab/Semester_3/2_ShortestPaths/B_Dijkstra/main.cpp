#include <algorithm>
#include <iostream>
#include <vector>
#include <array>
#include <queue>

using namespace std;

using _dtype = int;
using _dvpair = std::pair<_dtype, int>;
int n, m;
std::array<_dtype, 30001> d;
std::array<std::vector<_dvpair>, 30001> adj;

void read() {
    int a, b, w;
    std::cin >> n >> m;
    for(int i = 0; i < m; i++) {
        std::cin >> a >> b >> w;
        --a; --b;
        adj[a].push_back({w,b});
        adj[b].push_back({w,a});
    }
    std::fill(d.begin(), d.end(), std::numeric_limits<_dtype>::max() / 2);
}

void dijkstra() {
    std::priority_queue<_dvpair, std::vector<_dvpair>, std::greater<_dvpair>> q;
    q.push({0, 0});
    d[0] = 0;
    for(int i = 0; i < n; i++) {
        _dvpair top;
        do {
           top = q.top();
           q.pop();
        } while(top.first != d[top.second]);

        //std::cout << "Selected vertex " << (top.second + 1) << " with d = " << top.first << " / " << d[top.second] << std::endl;

        auto vertex = top.second;
        for(auto e : adj[vertex]) {
            _dtype m = min(d[e.second], d[vertex] + e.first);
            if (d[e.second] > m) {
                d[e.second] = m;
                q.push({d[e.second], e.second});
                //std::cout << "    Update v = " << (e.second + 1) << ", d = " << d[e.second] << std::endl;
            }
        }
    }
}

int main()
{
    read();
    dijkstra();
    for(int i = 0; i < n; i++) {
        std::cout << d[i] << " ";
    }
    std::cout << std::endl;
    return 0;
}
