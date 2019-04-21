#include <algorithm>
#include <array>
#include <iostream>
#include <vector>

const int N = 301;
std::array<std::array<int, N>, N> a;
std::array<int, N> u, v;
std::array<bool, N> col_used;

int main() {
    int n;
    std::cin >> n;

    for(int i = 0; i < n; ++i) {
        for(int j = 0; j < n; ++j) {
            std::cin >> a[i][j];
        }
    }

    for(int row = 0; row < n; ++row) {
        auto mincol = std::min_element(a[row].begin(), a[row].end()) - a[row].begin();
        if (!col_used[mincol]) {
            col_used[mincol] = 1;
        }

     
    }



}
