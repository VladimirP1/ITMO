#pragma once
#include <iostream>

using _integer_type = unsigned long long;
using _integer_type_signed = long long;

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

std::ostream& operator<< (std::ostream& out, const RationalNumber& num);
std::istream& operator>> (std::ostream& in, RationalNumber& num);

