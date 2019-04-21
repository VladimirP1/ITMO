#include <lib_poly.h>
#include <iostream>
#include <vector>

void testLibPoly(std::vector<RationalNumber> cooficients, RationalNumber p1, RationalNumber p2, bool expected) {
    RationalPoly poly(cooficients); 
    if (poly.comparePolyInPoints(p1, p2) != expected) { 
		std::cerr << "Fail at :" << std::endl; 
		for (auto c : cooficients) {
			std::cerr << c << std::endl;
		}
		std::cerr << "p1 = " << p1 << std::endl;
		std::cerr << "p2 = " << p2 << std::endl;
		exit(1);
	} 
}

int main(){
	testLibPoly({1, 0}, 0, 1, 1);
	testLibPoly({0, 1}, 0, 1, 0);
	testLibPoly({1, 0, 0}, 0, 1, 1);
	testLibPoly({1, 0, 1}, 0, 1, 1);
	testLibPoly({1, 0, {4, 3}}, 0, 1, 1);
	testLibPoly({{3, 4}, 0, {4, 3}}, 0, 1, 1);
	testLibPoly({{1, 100}, 0, 0}, 0, 1, 1);

    return 0;
}
