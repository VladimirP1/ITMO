#include <algorithm>
#include <array>
#include <iostream>
#include <fstream>
#include <vector>

const unsigned int P = 37;
using _htype = unsigned int;

std::string s;
std::array<_htype, 100005> h;
std::array<int, 100005> powers;

void precalcPowers() {
    _htype x = 1;
    for(auto& i : powers) {
        i = x;
        x *= P;
    }
}

void read() {
    std::cin >> s;
}

void precalcPrefHashes() {
    _htype hash = 0;
    for (int i = 0; i < s.size(); i++) {
        hash = hash * P + (_htype) s[i];
        h[i] = hash;
    }
}

_htype substrHash(int i, int j) {
    return h[j - 1] - powers[j - i] * h[i - 1];
}

int main() {
    precalcPowers();
    read();
    precalcPrefHashes();
    int m; std::cin >> m;
    while(m--) {
        int t1, t2, s1, s2;
        std::cin >> t1 >> t2 >> s1 >> s2;
        --t1; --s1;
        if (t2 - t1 != s2 - s1) { std::cout << "No\n"; continue; }
        std::cout << ((substrHash(t1, t2) == substrHash(s1, s2)) ? "Yes\n" : "No\n");
    }
    std::cout << std::flush;
    return 0;
}
