int demo11(int a) {
    return a*a;
}

// We are overloading demo01 here
// c compiler would fail, c++ does not
double demo11(double a) {
    return a*a;
}

// Return type overloading is impossible
//     double demo01(int a) {
//         return a*a;
//     }
