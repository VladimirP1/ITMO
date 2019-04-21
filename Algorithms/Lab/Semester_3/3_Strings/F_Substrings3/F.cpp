#include <algorithm>
#include <array>
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using _htype = unsigned int;

class PowerClass {
public:
    static void precalculate(size_t n) {
        powers.resize(std::max(powers.size(), n));

        _htype x = 1;
        for(auto& i : powers) {
            i = x;
            x *= P;
        }
    }
    static inline const _htype& get(size_t x) {
        return powers[x];
    }
    static inline const _htype& getP() {
        return P;
    }
private:
    static const _htype P;
    static std::vector<_htype> powers;
};

const _htype PowerClass::P = 37;
std::vector<_htype> PowerClass::powers;

class HashedStr {
public:
    HashedStr() {}
    HashedStr(const std::string& s) {
        precalculate(s); 
        zFunc = z(s);
        str = s;
    }
   _htype substrHash(int i, int j) const {
       return hash[j - 1] - PowerClass::get(j - i) * hash[i - 1];
   }
   const std::string& getStr() const {
        return str;
   }
   const int getZ(int pos) const { return zFunc[pos]; }

private:
    std::string str;
    std::vector<int> hash;
    std::vector<int> zFunc;
    void precalculate(const std::string& s) {
        _htype hash = 0;
        this->hash.resize(s.size());
        for (int i = 0; i < s.size(); i++) {
            hash = hash * PowerClass::getP() + (_htype) s[i];
            this->hash[i] = hash;
        }
    }
    int z_naive(std::string& s, int i, int z) {
        auto n = s.size();
        for (int j = z; j < n - i + 1; j++) {
            if(i + j >= n or s[i + j] != s[j]) {
                return j;
            }
        }
        
        return 0;
    }
    
    std::vector<int> z(std::string s) {
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
};


int main() {
    PowerClass::precalculate(10005);

    int K;
    std::string tmp;
    std::vector<HashedStr> strs;

    std::cin >> K;

    for (int i = 0; i < K; i++) {
        std::cin >> tmp;
        strs.push_back(HashedStr(tmp));
    }

    std::sort(strs.begin(), strs.end(), [](const HashedStr& a, const HashedStr& b) { return a.getStr().size() < b.getStr().size(); });

    

    return 0;
}

