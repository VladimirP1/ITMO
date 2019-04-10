#include <algorithm>
#include <iostream>
#include <vector>
#include <cassert>

template<int MOD>
class ffint {
public:
    ffint() {}
    ffint(int x) : value(x) {}

    int getValue() const { return value; }
    
    ffint& operator+=(const ffint& other) {
        value = (((int64_t)value) + ((int64_t)MOD) + ((int64_t)other.getValue())) % MOD;
        return *this;
    }

    ffint& operator-=(const ffint& other) {
        return *this += ffint(-other.getValue());
    }

    ffint& operator*=(const ffint& other) {
        value = (((int64_t)value) * ((int64_t)other.getValue()) + ((int64_t)MOD)) % MOD;
        return *this;
    }

    ffint& operator/=(const ffint& other) {
        return *this *= inv_(other.getValue());
    }

    ffint operator+(const ffint& other) const {
        return ffint(*this) += other;
    }

    ffint operator-(const ffint& other) const {
        return ffint(*this) -= other;
    }
    
    ffint operator*(const ffint& other) const {
        return ffint(*this) *= other;
    }
    
    ffint operator/(const ffint& other) const {
        return ffint(*this) /= other;
    }

    bool operator==(const ffint& other) const {
        return getValue() == other.getValue();
    }

    static int pow_(int number, int degree) {
        if (degree == 0) return 1;

        if (degree % 2 == 0) {
            int x = pow_(number, degree / 2);
            return (((int64_t)x) * ((int64_t)x)) % MOD;
        } else {
            return (((int64_t)number) * ((int64_t)pow_(number, degree - 1))) % MOD;
        }
    }

    static int inv_(int number) {
        return pow_(number, MOD - 2);
    }

#define TEST(test) \
    bool result; \
    std::cout << "Testing: " << std::endl \
    << #test << std::endl \
    << "Result: " << (result=(test)) << std::endl; \
    assert(result);

    static void check() {
        ffint a, b, c;
        a = {1};
        b = {0};
        assert(b - a == MOD - 1);
        assert(a == 1);
        assert(b == 0);
        assert((a += 1) == 2);
        assert(a == 2);
        assert((a *= 2) == 4);
        assert(a == 4);
        assert((a -= 3) == 1);
        assert(a-1 == 0);
    }
private:
    int value {0};
};

using ffintm = ffint<998244353>;
using poly = std::vector<ffintm>;

class polynom {
public:
    polynom() { }
    polynom(const std::initializer_list<ffintm>& lst) : p(lst) { }

    polynom& operator+=(const polynom& other) {
        p.resize(std::max(p.size(), other.p.size()));

        for (size_t i = 0; i < other.p.size(); ++i) {
            p[i] += other.p[i];
        }
        
        return *this;
    }

    polynom& operator-=(const polynom& other) {
        p.resize(std::max(p.size(), other.p.size()));

        for (size_t i = 0; i < other.p.size(); ++i) {
            p[i] -= other.p[i];
        }
        
        return *this;
    }

    polynom& operator*=(const polynom& rhs) {     
        polynom lhs(*this); 
        p.resize(lhs.p.size() + rhs.p.size());

        for (int i = 0; i < p.size(); ++i) {
            p[i] = 0; 
            for (
                    int j = std::max(1 + i - (int) rhs.p.size(), 0);
                    j <= i && j < lhs.p.size();
                    ++j
                            ) { 
                p[i] += lhs.p[j] * rhs.p[i - j];
            }
        }
        return *this;
    }

    polynom& operator/=(const ffintm& other) {
        for_each(p.begin(), p.end(), [&other](ffintm& x){x/=other;});
        return *this;
    }
    
    polynom operator+(const polynom& other) const {
        return polynom(*this) += other;
    }
    
    polynom operator*(const polynom& other) const {
        return polynom(*this) *= other;
    }

    polynom operator/(const ffintm& other) const {
        return polynom(*this) /= other;
    }

    polynom& minify() {
        while(!p.empty() && p.back() == 0) {
            p.pop_back();
        }
        return *this;
    }

    bool operator==(const polynom& other) {
        polynom lhs(*this);
        polynom rhs(other);
        lhs.minify();
        rhs.minify();
        return lhs.p == rhs.p;
    }

    polynom ln1p(size_t demand) const {

        polynom dp {*this};
        polynom result({0});
        
        for (size_t i = 1; i < demand + 1; ++i) {
            if (i & 1) {
                result += dp/i;
            } else {
                result -= dp/i;
            }
            dp *= (*this);
            dp.minify();
        }
        
        result.p.resize(demand);

        return result;
    }

    polynom exp(size_t demand) const {

        ffintm fac{1};
        polynom dp {*this};
        polynom result({1});
        
        for (size_t i = 1; i < demand + 1; ++i) {
            fac *= i;
            result += (dp / fac);
            dp *= *this;
            dp.minify();
        }
        
        result.p.resize(demand);

        return result;
    }

    polynom sqrt1p(size_t demand) const {

        ffintm fac{1};
        ffintm mult{1};
        ffintm div{2};
        polynom dp {*this};
        polynom result({1});
        
        for (size_t i = 1; i < demand + 1; ++i) {
            fac *= i;
            result += ((((dp / fac)) *= polynom {mult}) /= div);
            dp *= *this;
            dp.minify();
            mult *= (ffintm(1) - ffintm(2) * ffintm(i));
            div *= 2;
        }
        
        result.p.resize(demand);

        return result;
    }

    static void check() {
        polynom a {1,0,0};
        polynom b {0,1,0};
        assert(a*b == polynom({0,1}));
        assert(a+b == polynom({1,1}));
    }
    
    friend void read(std::istream& in, polynom& p, size_t power); 
    friend std::ostream& operator<<(std::ostream& out, const polynom& p);
private:
    poly p;
};

void read(std::istream& in, polynom& p, size_t power) {
    p.p.resize(power + 1);

    for (size_t i = 0; i <= power; ++i) {
        int n;
        in >> n;
        p.p[i] = {n};
    }
}

std::ostream& operator<<(std::ostream& out, const polynom& p) {
    for (const auto& k : p.p) {
        out << k.getValue() << " ";
    }
    return out;
}

int main() {

    ffint<12>::check();
    polynom::check();

    int n, m;
    std::cin >> n >> m;

    polynom p;
    read(std::cin, p, n);
    std::cout << p.sqrt1p(m) << std::endl <<
                 p.exp(m) << std::endl <<
                 p.ln1p(m) << std::endl;

}
