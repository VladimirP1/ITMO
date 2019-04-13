#include <iostream>
#include <vector>
#include <algorithm>

using poly = std::vector<int64_t>;

const int64_t MOD = 104857601;

#define ADD(x,y) ((((int64_t)(x)) + ((int64_t)(y)) + ((int64_t)(MOD))) % MOD)

poly pp(poly in, size_t n) {
    poly ret;
    ret.reserve(in.size());

    for (size_t i = 0; i < in.size(); ++i) {
        if ((i&1) == (n&1)) {
            ret.push_back(in[i]);
        }
    }

    return ret;
}

poly mult(const poly& lhs, const poly& rhs) {     
    poly p;
    p.resize(lhs.size() + rhs.size());

    for (int i = 0; i < p.size(); ++i) {
        p[i] = 0; 
        for (
                int j = std::max(1 + i - (int) rhs.size(), 0);
                j <= i && j < lhs.size();
                ++j
                        ) { 
            p[i] = ADD(p[i], lhs[j] * rhs[i - j]);
        }
    }
    return p;
}

poly negateOdd(poly p) {
    poly ret;
    ret.resize(p.size());

    for (int i = 0; i < p.size(); ++i) {
        ret[i] = (i&1) ? ADD(-p[i], 0) : ADD(p[i], 0);
    }

    return ret;
}

template<class I>
std::ostream& print(std::ostream& out, const I& lst) {
    for (auto&x : lst) {
        out << x << " ";
    }
    return out;
}

auto calc_n(size_t n, poly init, poly rel) {
    while (n >= rel.size() - 1) {
        init.resize(2 * init.size());

        for (int i = rel.size() - 1; i < (rel.size() - 1) * 2; ++i) {
            for (int j = 1; j <= (rel.size() - 1); ++j) {
                init[i] = ADD(init[i], -init[i - j] * rel[j]);
                while(init[i] < 0) init[i] += MOD;
            }
        }
       
        poly rel2 = mult(rel, negateOdd(rel));

        init = pp(init, n);
        rel = poly(pp(rel2, 0));

        n /= 2;
    }
    return init[n];
}

int main() {
    uint64_t k, n;

    std::cin >> k >> n;

    poly A, C;
    A.reserve(k);
    C.reserve(k);

    for (uint64_t i = 0, j; i < k; ++i) {
        std::cin >> j;
        A.push_back(j);
    }

    C.push_back(1);
    for (uint64_t i = 0, j; i < k; ++i) {
        std::cin >> j;
        C.push_back(ADD(-j, 0));
    }

    std::cout << calc_n(n - 1, A, C) << std::endl;
}
