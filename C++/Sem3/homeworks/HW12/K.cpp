#pragma GCC optimize ("O3")
#include <ext/rope>
#include <algorithm>
#include <iostream>
#include <numeric>
#include <list>
#include <vector>

int main() {

    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    
    int n, m, t1, t2;

    std::cin >> n >> m;

    std::vector<int> base(n);
    std::iota(base.begin(), base.end(), 1);
    __gnu_cxx::rope<int> s(&base[0], n);

    for(int i = 0; i < m; i++) {
        std::cin >> t1 >> t2;
        --t1;
        auto x = s.substr(t1, t2 - t1);
        s.erase(t1, t2 - t1);
        s = x + s;
    }

    std::for_each(s.begin(), s.end(), [](int x){std::cout << x << " ";});
    std::cout << std::endl;
    
    return 0;
}
