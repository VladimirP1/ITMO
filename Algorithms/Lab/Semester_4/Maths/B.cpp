#pragma GCC target("avx")
#pragma GCC optimize("Ofast")
#pragma GCC optimize("inline")

#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

std::vector<int> sieve;

void generate() {
    sieve.resize(1000006);
    std::fill(sieve.begin(), sieve.end(), 1);

    for (int i = 2; i*i <= sieve.size(); ++i) if (sieve[i] == 1) {
        int z = i*i;
        while(z < sieve.size()) {
            sieve[z] = i;
            z += i;
        }
    }
}

std::vector<int> divisors(int x) {
    std::vector<int> result;

    while(sieve[x] != 1) {
        result.push_back(sieve[x]);
        x /= sieve[x];
    }

    if (sieve[x] == 1 && x != 1) result.push_back(x);


    std::sort(result.begin(), result.end());

    return result;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    int n;
    uint64_t x;

    generate();

    std::cin >> n;
    while (n--) {
        std::cin >> x;
        auto d = divisors(x);
        for (auto y : d) {
            printf("%d ", y);
        }
        printf("\n");
    }
}
