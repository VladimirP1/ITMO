#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

int z_naive(std::string& s, int i, int z) {
    auto n = s.size();
    for (int j = z; j < n - i + 1; j++) {
        if(i + j >= n or s[i + j] != s[j]) {
            return j;
        }
    }
    
    return 0;
}

auto z(std::string s) {
    auto n = s.size();
    auto ans = std::vector<int>(n);
    auto l = 0;
    auto r = 0;
    for (int i = 1; i < n; i++) {
        if (i > r) {
            ans[i] = z_naive(s, i, 0);
            l = i;
            r = ans[i] + i - 1;
        } else {
            int init = std::min(r - i + 1, ans[i - l]);
            ans[i] = z_naive(s, i, init);
        }
        if (ans[i] + i - 1 > r) {
            l = i;
            r = ans[i] + i - 1;
        }
    }
    return ans;
}

int main() {
    std::string p, t;
    std::vector<int> matches;
    
    std::ios_base::sync_with_stdio(false);
    std::cin >> p >> t; 
    
    //Search for p in t
    
    std::string zuString = p + "#" + t;
    auto v = z(zuString);

    for (int i = 0, j; i < t.size(); i++) {
        j = i + p.size() + 1;
        if(v[j] >= p.size()) {
            matches.push_back(i);
        }
    }
    std::cout << matches.size() << std::endl;
    std::for_each(matches.begin(), matches.end(), [](int x){std::cout << x + 1 << " ";});
    std::cout << std::endl;
    return 0;
}

