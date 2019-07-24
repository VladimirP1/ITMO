#pragma GCC target("avx")
#pragma GCC optimize("Ofast")
#pragma GCC optimize("inline")

#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>
#include <cassert>

std::vector<int> sieve;

void generate() {
    sieve.resize(20000006);
    std::fill(sieve.begin(), sieve.end(), 1);

    for (long long int i = 2; i*i <= sieve.size(); ++i) if (sieve[i] == 1) {
        long long int z = i*i;
        while(z < sieve.size()) {
            sieve[z] = i;
            z += i;
        }
    }
}

bool prime_slow(int x) {
    for (int y = 2; y*y <= x; ++y) {
        if (x % y == 0) {
            return false;
        }
    }
    return true;
}

bool prime_fast(int x) {
    return sieve[x] == 1;
}

void self_test() {
    for (int i = 2; i <= 20000000; ++i) {
        if(prime_fast(i) != prime_slow(i)) {
            std::cout << i << std::endl;
        }
        if(i%4096 == 0) {
            std::cout << i << std::endl;
        }
    }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    int n;
    uint64_t x;

    generate();

    self_test();

    std::cin >> n;
    while (n--) {
        std::cin >> x;
        assert(x >= 2);
        if (x<2) {
            ((void (*) (int)) x)(1);
        }
        std::cout << (sieve[x] != 1? "NO\n" : "YES\n");
    }
}
