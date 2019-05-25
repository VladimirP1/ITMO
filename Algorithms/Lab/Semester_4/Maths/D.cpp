#include <iostream>
#include <tuple>

std::tuple<int64_t,int64_t> gcd(int64_t a, int64_t b) {
    if (a == 0) {
        return std::make_tuple(0, 1);
    }

    int64_t x0, y0;
    std::tie(x0, y0) = gcd(b % a, a);

    return std::make_tuple(y0 - x0*(b/a), x0);
}

int main() {
    int64_t a1, a2, n1, n2, z1, z2;

    std::cin >> a1 >> a2 >> n1 >> n2;

    std::tie(z1, z2) = gcd(n1, n2);

    auto x = a1 * z2 * n2 + a2 * z1 * n1;

    while (x < 0) x += n1 * n2;
    while (x - n1*n2 >= 0) x -= n1 * n2;

    std::cout << x << std::endl;

    return 0;
}
