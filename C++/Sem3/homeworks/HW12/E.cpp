#pragma GCC optimize ("O3")
#include <iostream>
#include <algorithm> 
#include <vector>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    
    std::vector<int> a;
    int n, k, A, B, C, a1, a2;

    std::cin >> n >> k >> A >> B >> C >> a1 >> a2;

    a.resize(n);
    a[0] = a1;
    a[1] = a2;

    for (int i = 2; i < n; ++i) {
        a[i] = A * a[i - 2] + B * a[i - 1] + C;
    }

    auto partition = a.begin() + k - 1;
    std::nth_element(a.begin(), partition, a.end());
    std::cout << *partition << std::endl;
    
    return 0;
}
