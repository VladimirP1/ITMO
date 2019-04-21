#include <lib_poly.h>
#include <vector>
#include <algorithm>
#include <iostream>

int main() {
	int degree = 0;
	std::vector<RationalNumber> cooficients;
	RationalNumber point1, point2;
	std::cout << "Enter your polynomial's degree:" << std::endl;
	std::cin >> degree;
    std::cout << "Enter its cooficients starting from he highest power:" << std::endl;
	
	for(int i = 0; i <= degree; i++) {
		cooficients.push_back(0);
		std::cin >> cooficients[i];
	}   
    
    std::cout << "Enter point 1" << std::endl;
    std::cin >> point1;
    std::cout << "Enter point 2" << std::endl;
    std::cin >> point2;
    
    RationalPoly polynomial(cooficients);    
    std::cout << "f(p1) < f(p2) = " << polynomial.comparePolyInPoints(point1, point2) << std::endl;
}
