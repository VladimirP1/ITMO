#include "lib_rational.h"
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
    return lhs;
}

RationalNumber& RationalNumber::operator-= (const RationalNumber& b) {
    RationalNumber& lhs = *this;
    RationalNumber  rhs = b;
    toCommonDenominator(lhs, rhs);
    lhs.mNumerator -= rhs.mNumerator;
    return lhs;
}

RationalNumber& RationalNumber::operator*= (const RationalNumber& b) {
    mNumerator *= b.mNumerator;
    mDenominator *= b.mDenominator;
    return *this;
}

RationalNumber& RationalNumber::operator/= (const RationalNumber& b) {
    mNumerator *= b.mDenominator;
    mDenominator *= llabs(b.mNumerator);
    if (b.mNumerator < 0) mNumerator = -mNumerator;
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
