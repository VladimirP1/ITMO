#include <lib_rational.h>
#include <iostream>

void testAddition(RationalNumber a, RationalNumber b, RationalNumber expected) {
	RationalNumber result = a + b;
	if (result != expected) {
		std::cerr << "Fail: " << a << " + " << b << " is " << result << " expecting " << expected << std::endl;
		exit(1);
	}
}

void testReduction(RationalNumber input, RationalNumber expected) {
	input.reduce();
	if(input != expected) {
		std::cerr << "Fail: have " << input << " expecting " << expected << std::endl;
	}
}

int main(){
    testAddition(1, 2, 3);
    testAddition({1, 2}, {2, 3}, {7, 6});
    testAddition({1, 2}, {-2, 3}, {-1, 6});
    testAddition({4000, 5}, {-800, 1}, 0);
    
    testReduction(5, 5);
    testReduction(-5, -5);
    testReduction({100, 100}, 1);
    testReduction({-100, 100}, -1);
    testReduction({-555, 5}, -111);

    
    return 0;
}
