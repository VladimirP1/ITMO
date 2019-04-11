#include <iostream>
#include <algorithm>
#include <vector>

bool compare(int a, int b) {
    std::string s;
    std::cout << 1 << " " << (a + 1) << " " << (b + 1) << std::endl;
    std::cin >> s;
    return s[0] == 'Y';
}

int main()
{
    int n;
    std::cin >> n;
    std::vector<int> lamps;

    for(int i = 0; i < n ; i ++) {
        auto ins = std::upper_bound(lamps.begin(), lamps.end(), i, [](int a, int b){return compare(a,b);});
        lamps.insert(ins, i);
    }


    for(int i = 1; i < n; i++) {
        //std::cout << "Compare at (" << i-1 << ", " << i << ")" << std::endl;
        if (!compare(lamps[i - 1], lamps[i])) {
            for(int j = 0; j < n + 1; j++) std::cout << "0 ";
            std::cout << std::endl;
            return 0;
        }
    }

    std::cout << 0 << " ";

    for(int i = 0; i < n; i++) {
        std::cout << (lamps[i] + 1) << " ";
    }

    std::cout << std::endl;


    return 0;
}
