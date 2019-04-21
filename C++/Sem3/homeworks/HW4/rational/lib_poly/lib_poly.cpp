#include "lib_poly.h"

RationalPoly::RationalPoly(std::vector<RationalNumber> coeficients) : mCoeficients(coeficients) {
    
}

RationalNumber RationalPoly::calculate(RationalNumber point) {
    
    RationalNumber result = mCoeficients[0];
    for (int i = 1; i < mCoeficients.size(); ++i) {
        result *= point;
        result += mCoeficients[i];
   }
   return result;
}

bool RationalPoly::comparePolyInPoints(RationalNumber left, RationalNumber right) {
    return (calculate(left) < calculate(right));
}
