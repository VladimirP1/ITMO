#include <vector>
#include <iostream>

const int64_t MOD = 1000000007LL;

int main() {
    int k, m;
    
    std::vector<int64_t> c;   // Allowed node weights
    std::vector<int64_t> x;   // Number of trees with weight i
    std::vector<int64_t> pwi; // Number of pairs of trees with weight i

    std::cin >> k >> m;
    
    c.resize(k);

    for (int i = 0; i < k; ++i) {
        std::cin >> c[i];
    }

    x.resize(2002);
    pwi.resize(2002);

    x[0] = 1;
    pwi[0] = 1; 

    for (int k = 1; k <= m; ++k) {
        // Calculate number of trees of weight k 
        for (auto cc : c) {
            if (cc > k) break;
            x[k] = (x[k] + pwi[k - cc]) % MOD;
        }

        // Calculate number of pairs [of trees] of weight k
        for (int i = 0; i <= k; ++i) {
            pwi[k] = (pwi[k] + x[i] * x[k - i]) % MOD;
        }

        //printf("pwi[%d] = %lu\n", k, pwi[k]);
        //printf("x  [%d] = %lu\n", k, x  [k]);

        std::cout << x[k] << " ";
    }
    std::cout << std::endl;
}
