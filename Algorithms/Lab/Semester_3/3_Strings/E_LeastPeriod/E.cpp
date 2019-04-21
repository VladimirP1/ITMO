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
    int i;
    std::string s;
    std::vector<int> matches;
    
    std::ios_base::sync_with_stdio(false);
    std::cin >> s; 
    
    auto v = z(s);
    
    for (i = 0; i <= s.size()/2; i++) {
        if ((v[i] == (s.size() - i))) goto found;    
    }

notfound:
    std::cout << s.size() << std::endl;
    return 0;

found:
    if (s.size() % i != 0) { goto notfound; }
    std::cout << i << std::endl;
    return 0;
}


