#include <algorithm>
#include <iostream>
#include <vector>
 
const int MOD = 998244353;
 
class Solution {
   public:
    std::vector<int> p1, p2;
 
    void read() {
        int d1, d2;
 
        std::cin >> d1 >> d2;
        ++d1;
        ++d2;
 
        p1.resize(d1);
        p2.resize(d2);
 
        std::for_each(p1.begin(), p1.end(), [](int& a) { std::cin >> a; });
        std::for_each(p2.begin(), p2.end(), [](int& a) { std::cin >> a; });
    }
 
    void eqs(std::vector<int>& a, std::vector<int>& b) {
        if (a.size() > b.size())
            b.resize(a.size(), 0);
        else if (b.size() > a.size())
            a.resize(b.size(), 0);
    }
 
    std::vector<int> sum(std::vector<int> a, std::vector<int> b) {
        eqs(a, b);
        std::transform(a.begin(), a.end(), b.begin(), a.begin(),
                       [](int a, int b) { return ((int64_t) a + (int64_t) b + (int64_t) MOD) % MOD; });
        while (!a.back()) a.pop_back();
        return a;
    }
 
    std::vector<int> prod(std::vector<int> a, std::vector<int> b) {
        eqs(a, b);
 
        a.resize(2 * a.size(), 0);
        b.resize(2 * b.size(), 0);
 
        std::vector<int> c(2 * a.size(), 0);
 
        for (size_t i = 0; i < b.size(); ++i) {
            for (size_t j = 0; j <= i; ++j) {
                c[i] = (c[i] + ((int64_t)a[j] * (int64_t)b[i - j]) + MOD) % MOD;
            }
        }
        while (!c.back()) c.pop_back();
        return c;
    }
 
    std::vector<int> inv(std::vector<int> a) {
        // A(s) = a_0 + a_1 * s + a_2 * s^2 + ...
        // B(s) = b_0 + b_1 * s + b_2 * s^2 + ...
        // A(s) * B(s) = 1
        // a_0 * b_0 = 1              -> b_0 = 1 / a_0
        // a_1 * b_0 + a_0 * b_1 = 0  -> b_1 = -(a_1 * b_0) / a_0
        // ...
        // b_n = (- \sum_{j=0}^{n-1} b_j * a_{n-j})
        // ...
 
        std::vector<int> b(2500);
        b[0] = 1 / a[0];
 
        for (int i = 1; i < b.size(); ++i) {
            int sum = 0;
             
            for (int j = 0; j < i; ++j) {
                if (i - j < a.size()) {
                    int64_t diff = ((int64_t) b[j]) * ((uint64_t) a[i - j]);
                    int64_t cur = sum + MOD;
                    sum = (cur - diff) % MOD;
                    sum %= MOD;
                }
            }
 
            b[i] = sum / a[0];
        }
 
        while (!b.back()) b.pop_back();
        return b;
    }
 
    void printSequence(std::vector<int> seq) {
        std::cout << std::max((size_t)0, seq.size() - 1) << std::endl;
        std::for_each(seq.begin(), seq.end(),
                      [](int x) { std::cout << x << " "; });
        std::cout << std::endl;
    }
};
 
int main() {
    Solution s;
 
    s.read();
 
    auto sum = s.sum(s.p1, s.p2);
    auto prod = s.prod(s.p1, s.p2);
 
    auto divs = s.prod(s.p1, s.inv(s.p2));
    divs.resize(1000);
 
    s.printSequence(sum);
    s.printSequence(prod);
 
    std::for_each(divs.begin(), divs.end(),
                  [](int x) { std::cout << x << " "; });
    std::cout << std::endl;
}
