#pragma GCC optimize ("O3")
#include <vector>
#include <iostream>
#include <algorithm>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);

    int n, m;
    std::vector<int> a, q;

    std::cin >> n;
    a.resize(n);
    std::generate_n(a.begin(), n, [](){int x; std::cin >> x; return x;});

    std::cin >> m;

    auto query = [&a](int arg) {
        auto eqRange = std::equal_range(a.begin(), a.end(), arg);

        if (eqRange.first == eqRange.second) {
            std::cout << "-1 -1 \n";
            return;
        }

        std::cout << 
            eqRange.first - a.begin() + 1 <<
            ' ' <<
            eqRange.second - a.begin() <<
            '\n';
        
    };

    for(int i = 0, x; i < m; i++) {
        std::cin >> x;
        query(x);
    }
    std::cout << std::flush; 
    
    return 0;    
}
