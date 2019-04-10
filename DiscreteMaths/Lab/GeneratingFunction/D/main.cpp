#include <cmath>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

using _integer_type = uint64_t;
using _integer_type_signed = int64_t;

class RationalNumber {
public:
    RationalNumber (_integer_type_signed numerator, _integer_type denominator);
    RationalNumber (_integer_type_signed numerator);
    RationalNumber ();
    
    void reduce ();
    RationalNumber reduced () const;
    RationalNumber& operator+= (const RationalNumber& b);
    RationalNumber& operator-= (const RationalNumber& b);
    RationalNumber& operator*= (const RationalNumber& b);
    RationalNumber& operator/= (const RationalNumber& b);
    RationalNumber operator+ (const RationalNumber& b) const;
    RationalNumber operator- (const RationalNumber& b) const;
    RationalNumber operator* (const RationalNumber& b) const;
    RationalNumber operator/ (const RationalNumber& b) const;
    bool operator< (const RationalNumber& b) const;
    bool operator> (const RationalNumber& b) const;
    bool operator<= (const RationalNumber& b) const;
    bool operator>= (const RationalNumber& b) const;
    bool operator== (const RationalNumber& b) const;
    bool operator!= (const RationalNumber& b) const;
    
    RationalNumber& operator= (const _integer_type_signed & i);
    
    _integer_type_signed getNumerator() const;
    _integer_type getDenominator() const;
    
    friend std::istream& operator>> (std::istream& in, RationalNumber& num);
private:
    _integer_type_signed mNumerator;
    _integer_type mDenominator;
    
    _integer_type gcd(_integer_type a, _integer_type b) const;
    _integer_type lcm(_integer_type a, _integer_type b) const;
    void toCommonDenominator(RationalNumber& a, RationalNumber& b);
};

#include <utility>
#include <cstdlib>

RationalNumber::RationalNumber (_integer_type_signed numerator, _integer_type denominator) :
        mNumerator(numerator),
        mDenominator(denominator) {}
        
RationalNumber::RationalNumber (_integer_type_signed numerator) :
        mNumerator(numerator),
        mDenominator(1) {}
        
RationalNumber::RationalNumber () :
        mNumerator(0),
        mDenominator(1) {}
        
void RationalNumber::reduce () {
    _integer_type_signed gcd = this->gcd(llabs(mNumerator), mDenominator);
    mNumerator /= gcd;
    mDenominator /= gcd;
}

RationalNumber RationalNumber::reduced () const {
    RationalNumber ret = *this;
    ret.reduce();
    return ret;
}

RationalNumber& RationalNumber::operator+= (const RationalNumber& b) {
    RationalNumber& lhs = *this;
    RationalNumber  rhs = b;
    toCommonDenominator(lhs, rhs);
    lhs.mNumerator += rhs.mNumerator;
    lhs.reduce();
    return lhs;
}

RationalNumber& RationalNumber::operator-= (const RationalNumber& b) {
    RationalNumber& lhs = *this;
    RationalNumber  rhs = b;
    toCommonDenominator(lhs, rhs);
    lhs.mNumerator -= rhs.mNumerator;
    lhs.reduce();
    return lhs;
}

RationalNumber& RationalNumber::operator*= (const RationalNumber& b) {
    mNumerator *= b.mNumerator;
    mDenominator *= b.mDenominator;
    this->reduce();
    return *this;
}

RationalNumber& RationalNumber::operator/= (const RationalNumber& b) {
    mNumerator *= b.mDenominator;
    mDenominator *= llabs(b.mNumerator);
    if (b.mNumerator < 0) mNumerator = -mNumerator;
    this->reduce();
    return *this;
}

RationalNumber RationalNumber::operator+ (const RationalNumber& b) const {
    RationalNumber ret = *this;
    return ret += b;
}

RationalNumber RationalNumber::operator- (const RationalNumber& b) const {
    RationalNumber ret = *this;
    return ret -= b;
}

RationalNumber RationalNumber::operator* (const RationalNumber& b) const {
    RationalNumber ret = *this;
    return ret *= b;
}

RationalNumber RationalNumber::operator/ (const RationalNumber& b) const {
    RationalNumber ret = *this;
    return ret /= b;
}

bool RationalNumber::operator< (const RationalNumber& b) const {
    RationalNumber res = (*this) - b;
    return res.mNumerator < 0;
}

bool RationalNumber::operator> (const RationalNumber& b) const {
    return ((*this) - b).mNumerator > 0;
}

bool RationalNumber::operator<= (const RationalNumber& b) const {
    return ((*this) - b).mNumerator <= 0;
}

bool RationalNumber::operator>= (const RationalNumber& b) const {
    return ((*this) - b).mNumerator >= 0;
}

bool RationalNumber::operator== (const RationalNumber& b) const {
    return ((*this) - b).mNumerator == 0;
}

bool RationalNumber::operator!= (const RationalNumber& b) const {
    return ((*this) - b).mNumerator != 0;
}

RationalNumber& RationalNumber::operator= (const _integer_type_signed & i) {
    mNumerator = i;
    mDenominator = 1;
    return *this;
}

_integer_type_signed  RationalNumber::getNumerator () const {
    return mNumerator;
}

_integer_type  RationalNumber::getDenominator () const {
    return mDenominator;
}

_integer_type RationalNumber::gcd (_integer_type a, _integer_type b) const {
    while (b) {
        a %= b;
        std::swap(a, b);
    }
    return a;
}

_integer_type RationalNumber::lcm (_integer_type a, _integer_type b) const {
    return a / gcd(a, b) * b;
}

void RationalNumber::toCommonDenominator (RationalNumber& a, RationalNumber& b) {
    _integer_type commonDenominator = lcm(a.mDenominator, b.mDenominator);
    a.mNumerator = commonDenominator / a.mDenominator * a.mNumerator;
    b.mNumerator = commonDenominator / b.mDenominator * b.mNumerator;
    a.mDenominator = b.mDenominator = commonDenominator;
}

std::ostream& operator<< (std::ostream& out, const RationalNumber& num) {
    out << num.getNumerator() << " / " << num.getDenominator();
    return out;
}

std::istream& operator>> (std::istream& in, RationalNumber& num) {
retry:
    in >> num.mNumerator;
    in >> num.mDenominator;
    if (num.mDenominator == 0) {
		std::cout << "Invalid denominator: 0. Please retry entry." << std::endl;
		goto retry;
	}
    return in;
}
std::ostream& operator<< (std::ostream& out, const RationalNumber& num);
std::istream& operator>> (std::ostream& in, RationalNumber& num);

using ffintm = RationalNumber;
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

    polynom& operator*=(const ffintm& other) {
        for_each(p.begin(), p.end(), [&other](ffintm& x){x*=other;});
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

    polynom operator*(const ffintm& other) const {
        return polynom(*this) *= other;
    }

    polynom operator/(const ffintm& other) const {
        return polynom(*this) /= other;
    }

    polynom& minify() {
        while(!p.empty() && p.back() == 0) {
            p.pop_back();
        }
        for(auto& x : p) {
            x.reduce();
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

    polynom list(size_t demand) {
        if(!p.empty()) {
            p[0] = ffintm{0};
        }

        polynom po (*this);
        polynom ret {0};
        for (int i = 0; i < demand; ++i) {
            ret += po;
            po *= (*this);
        }
        if(ret.p.empty()) ret.p.push_back(0);
        ret.p[0] = 1;
        return ret;
    }

    polynom pp(size_t demand, size_t pz) const {
        polynom ret;
        ret.p.resize(demand, 0);

        for (size_t i = 0; i < demand; i += pz) {
            if (i/pz < p.size()) {
                ret.p[i] = p[i/pz];
            }
        }
        return ret;
    }

    polynom mset(size_t demand) {
        polynom ret {0};

        if(!p.empty()) {
            p[0] = ffintm{0};
        }
        
        for (int i = 1; i < demand; ++i) {
            ret += this->pp(demand, i) / i;
        }


        return ret.exp(demand);
    }

    polynom exp(size_t demand) {

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

    static void check() {
        polynom a {1,0,0};
        polynom b {0,1,0};
        assert(a*b == polynom({0,1}));
        assert(a+b == polynom({1,1}));
    }

    polynom& trunc(int x) {
        p.resize(x + 1);
        return *this;
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
        p.p[i] = {(ffintm)n};
    }
}

std::ostream& operator<<(std::ostream& out, const polynom& p) {
    for (const auto& k : p.p) {
        out << k.getNumerator()/k.getDenominator() << " ";
    }
    return out;
}

size_t find_end(const std::string& x, size_t pos) {
    size_t lvl = 1;
    for(;lvl > 0; ++pos) {
        if(x[pos] == '(') ++lvl;
        else if(x[pos] == ')') --lvl;
    }
    return pos;
}

std::pair<size_t, size_t> find_end2(const std::string& x, size_t pos) {
    size_t lvl = 1;
    size_t rem = 0;
    for(;lvl > 0; ++pos) {
        if(x[pos] == '(') ++lvl;
        else if(x[pos] == ')') --lvl;
        if(x[pos] == ',' && lvl == 1) rem = pos;
    }

    return {rem, pos};
}

namespace calc {
const int trunc = 8;
polynom calculate(std::string s) {
    //std::cout << s << std::endl;
    if (s[0] == 'L') {
        return calculate(s.substr(2,find_end(s,2) - 3)).list(trunc).trunc(trunc);
    } else if (s[0] == 'S') {
        return calculate(s.substr(2,find_end(s,2) - 3)).mset(trunc).trunc(trunc);
    } else if (s[0] == 'P') {
        auto x = find_end2(s,2);
        auto s1 = s.substr(2, x.first- 2);
        auto s2 = s.substr(x.first + 1, x.second - x.first - 2);

        return (calculate(s1) * calculate(s2));
    } else if (s[0] == 'B') {
        return polynom {0, 1};
    } else {
        assert(0);
    }
}
};

int main() {
    std::string s;
    std::getline(std::cin, s);
    assert(s.find(' ') == s.npos);
    std::cout << calc::calculate(s).trunc(6) << std::endl;

}
