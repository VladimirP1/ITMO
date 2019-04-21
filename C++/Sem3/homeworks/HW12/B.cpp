#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>

int factorial(int x) {
    for(int i = x; --i;) {
        x *= i;
    }
    return x;
}

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    int n;
    std::cin >> n;
    std::vector<int> permutation(n);
    std::iota(permutation.begin(), permutation.end(), 1);
    for (int i = factorial(n); i--;) {
        std::for_each(permutation.begin(), permutation.end(), [](int x){std::cout << x << " "; });
        std::cout << "\n";
        std::next_permutation(permutation.begin(), permutation.end());
    }
    std::cout << std::flush;
    return 0;
}
