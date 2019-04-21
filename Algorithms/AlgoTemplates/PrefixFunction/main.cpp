#include <algorithm>
#include <iostream>
#include <vector>
#include <string>

auto prefix(std::string s) {
    const int n = s.length();
    std::vector<int> p(n);
    for (int i = 1; i < n; i++) {
        int k; for (k=p[i-1];s[i]!=s[k]&&k>0;k=p[k-1]);
        if (s[i] == s[k]) ++k;
        p[i] = k;
    }
    return p;
}

int main() {
    std::string s;
    std::cin >> s;
    auto r = prefix(s);
    std::for_each(r.begin(), r.end(), [](int x){std::cout << x << " ";});
    std::cout << std::endl;
}
