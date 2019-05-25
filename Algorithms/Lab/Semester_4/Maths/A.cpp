#pragma GCC target("avx")
#pragma GCC optimize("Ofast")
#pragma GCC optimize("inline")
#include <vector>
#include <iostream>
#include <cmath>

void no(){std::cout << "NO\n";}
void yes(){std::cout << "YES\n";}

bool check_prime(uint64_t x) {
    if (x==1) return false;
    uint64_t i;
    for (i = 2; (i + 15) < (uint64_t) (sqrt(x)) && i + 16 < x; i += 16) {
        bool r = false;
        //std::cout << i << std::endl;
        r |= (x % (i + 0 )) == 0;
        r |= (x % (i + 1 )) == 0;
        r |= (x % (i + 2 )) == 0;
        r |= (x % (i + 3 )) == 0;
        r |= (x % (i + 4 )) == 0;
        r |= (x % (i + 5 )) == 0;
        r |= (x % (i + 6 )) == 0;
        r |= (x % (i + 7 )) == 0;
        r |= (x % (i + 8 )) == 0;
        r |= (x % (i + 9 )) == 0;
        r |= (x % (i + 10)) == 0;
        r |= (x % (i + 11)) == 0;
        r |= (x % (i + 12)) == 0;
        r |= (x % (i + 13)) == 0;
        r |= (x % (i + 14)) == 0;
        r |= (x % (i + 15)) == 0;
        if (r) return false; 
    }

    for(;i<(uint64_t) sqrt(x) + 2 && i < x; ++i) {
        if (x%i == 0) return false;
    }

    return true;
}

bool check_prime_slow(uint64_t x) {
    if (x == 1) return false;
    for (uint64_t i = 2; i < sqrt(x) + 4 && i < x; i++) {
        if (x%i==0) return false;
    }
    return true;
}

int main() {
    std::ios_base::sync_with_stdio(false);
    int n; std::cin >> n;
    uint64_t x;
    

    while (n--) {
        std::cin >> x;
        if (check_prime(x)) {
            yes();
        } else {
            no();
        }
    }
}
