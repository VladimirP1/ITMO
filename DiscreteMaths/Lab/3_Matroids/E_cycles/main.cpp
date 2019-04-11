#include <iostream>
#include <queue>
#include <vector>
#include <algorithm>
#include <cmath>
#include <fstream>

std::ifstream in("cycles.in");
std::ofstream out("cycles.out");
//std::istream& in = std::cin;
//std::ostream& out = std::cout;

using std::cin;
using std::cout;
using std::endl;

int n, m;
std::vector<std::pair<int, int>> weights;
std::vector<bool> I;
std::queue<int> C;

void read() {
    in >> n >> m;
    weights.resize(n);
    I.resize(1<<n);
    std::fill(I.begin(), I.end(), true);

    for(int i=0,tmp; i <n; i++) {
        in >> tmp;
        weights[i] = {tmp, i};
    }
    for(int i = 0; i < m; i++) {
        int l;
        in >> l;
        int smask = 0;
        for(int j = 0; j < l; j++) {
            int tmp;
            in >> tmp;
            --tmp;
            smask |= (1 << tmp);
        }
        I[smask] = false;
        C.push(smask);
    }
}

void findI() {
    std::queue<int> q;
    while (!C.empty()) {
        int cc = C.front();
        C.pop();
        for (int i = 0; i < n; i++) {
            if ((cc & (1<<i)) == 0) {
                if (I[cc | (1 << i)]) {
                    I[cc | (1 << i)] = false;
                    q.push(cc | (1 << i));
                }
            }
        }
    }
}
void greedy() {
    std::sort(weights.begin(), weights.end(), std::greater<std::pair<int, int>>());
    int curmask = 0;
    int64_t sum = 0;
    for (int i = 0; i < n; i++) {
        if (I[curmask | 1 << weights[i].second]) {
            curmask |= 1 << weights[i].second;
            sum += weights[i].first;
        }
    }
    out << sum << endl;
}

int main()
{
    read();
    findI();
    greedy();
    return 0;
}
