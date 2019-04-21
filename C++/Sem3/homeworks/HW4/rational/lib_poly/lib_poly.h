#pragma once
#include "lib_rational.h"
#include <vector>

class RationalPoly {
public:
    RationalPoly(std::vector<RationalNumber> coeficients);
    RationalNumber calculate(RationalNumber point);
    bool comparePolyInPoints(RationalNumber left, RationalNumber right);
private:
    std::vector<RationalNumber> mCoeficients;
    
};
