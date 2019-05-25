#include <cmath>
#include <iostream>
#include <tuple>
#include <vector>

std::tuple<int, int, int> gcd(int a, int b) {
    if (a == 0) {
        return std::make_tuple(0, 1, b);
    }

    int x0, y0, r0;
    std::tie(x0, y0, r0) = gcd(b % a, a);

    return std::make_tuple(y0 - x0 * (b / a), x0, r0);
}

int phi(int x) {
    int ret = x;
    for (int i = 2; i * i <= x; i++) {
        ret -= (x % i == 0) * ret / i;
        while (x % i == 0) { x /= i; }
    }
    ret -= (x > 1) * ret / x;
    return ret;
}

uint64_t power(uint64_t x, uint64_t y, uint64_t mod) {
    if (y == 0) return 1;
    if (y == 1) return x;

    if (y % 2 == 0) {
        uint64_t r1 = power(x, y / 2, mod);
        return r1 * r1 % mod;
    } else {
        return power(x, y - 1, mod) * x % mod;
    }
}


int main() {
    int n, e, d, c, phi_;

    std::cin >> n >> e >> c;

    phi_ = phi(n);

    std::tie(d, std::ignore, std::ignore) = gcd(e, phi_);

    while (d < 0) d += phi_;
    while (d - phi_ >= 0) d -= phi_;

    std::cout << power(c, d, n) << std::endl;

    return 0;
}
