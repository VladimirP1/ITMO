#include <algorithm>
#include <iostream>
#include <array>
#include <vector>

const int N = 100;
const int M = 100;
int n, m;

using _dtype = int;
using _dvtype = std::pair<_dtype, int>;

/*std::array<std::vector<_dvtype>, 100> inc;
std::array<std::vector<_dvtype>, 100> outg;*/

std::array<std::array<_dtype, N>, N> w;
std::array<_dtype, N> D;
std::array<_dtype, N> p;

void read() {
    std::cin >> n;
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            std::cin >> w[i][j];
        }
    }
}

void initFordBellman() {
    std::fill(p.begin(), p.end(), -1);
    std::fill(D.begin(), D.end(), std::numeric_limits<_dtype>::max() / 2);
    D[0] = 0;
}

void fordBellman() {
    for(int i = 0; i < n; i++) {
        for(int u = 0; u < n; u++) {
            for(int v = 0; v < n; v++) {
                if((w[v][u] != 100000) && (D[v] + w[v][u] < D[u])) {
                    p[u] = v;
                    D[u] = D[v] + w[v][u];
                }
            }
        }
    }
}

void printNegativeCycle(int v) {
    for(int i = 0; i < n; i++) {
        v = p[v];
    }

    auto start = v;
    std::vector<int> cycle;
    do {
        v = p[v];
        cycle.push_back(v + 1);
    } while(v != start);

    std::reverse(cycle.begin(), cycle.end());

    std::cout << "YES" << std::endl;
    std::cout << cycle.size() << std::endl;

    for(auto v : cycle) {
        std::cout << v << " ";
    }
    std::cout << std::endl;
}

int findNegativeCycle() {
    initFordBellman();
    fordBellman();
    auto D_old = D;
    auto p_old = p;
    fordBellman();
    auto m = std::mismatch(D.begin(), D.end(), D_old.begin());
    if(m.first == D.end()) {
        std::cout << "NO" << std::endl;
        exit(0);
    }
    auto it = m.first;
    printNegativeCycle(it - D.begin());
}

void write() {
    for(int i = 0; i < n; i++) {
            std::cout << D[i] << " ";
    }
    std::cout << std::endl;
}


int main()
{
    read();
    findNegativeCycle();
    return 0;
}
