#include <algorithm>
#include <iostream>
#include <vector>
#include <array>
#include <queue>

using namespace std;

using _dtype = long long;
using _dvpair = std::pair<_dtype, int>;
int n, m;
int a,b,c;
std::array<_dtype, 100000> d;
std::array<std::vector<_dvpair>, 100000> adj;

void read() {
    int A, B, w;
    std::cin >> n >> m;
    for(int i = 0; i < m; i++) {
        std::cin >> A >> B >> w;
        --A; --B;
        adj[A].push_back({w,B});
        adj[B].push_back({w,A});
    }
    std::cin >> a >> b >> c;
    --a;--b;--c;
}

void dijkstra(int s) {
    std::fill(d.begin(), d.end(), std::numeric_limits<_dtype>::max());
    d[s] = 0;

    std::priority_queue<_dvpair, std::vector<_dvpair>, std::greater<_dvpair>> q;
    q.push({0, s});

    for(int i = 0; i < n; i++) {
        _dvpair top;
        do {
           if(q.empty()) goto out;
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
    out:
    ;
}

_dtype dd[3];

int main()
{
    read();
    dijkstra(a);
    dd[0] = d[b];
    dd[1] = d[c];
    dijkstra(b);
    dd[2] = d[c];

    std::sort(dd, dd + 3);

    if(dd[1] == std::numeric_limits<_dtype>::max()) {
        std::cout << -1 << std::endl;
    } else {
        std::cout << dd[0] + dd[1] << std::endl;
    }

    /*for(int i = 0; i < n; i++) {
        std::cout << d[i] << " ";
    }
    std::cout << std::endl;*/
    return 0;
}
