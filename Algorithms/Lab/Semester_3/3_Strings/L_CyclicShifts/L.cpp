#include <cassert>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <array>
#include <string>
#include <numeric>
#include <functional>

int writeIdx = 0;

namespace SufArrayAndCo {

//#define DEBUG

constexpr int N = 400009;

std::array<int, N> eClass, eClassNext;
std::array<int, N> sortable;

using returned_iterator_type = std::array<int, N>::iterator;

int log2(unsigned int x) {
    int log = 0;
    while (x>>=1) log++;
    return log;
}

returned_iterator_type buildSufArray(std::string s) {
    for (int i = 0; i < s.size(); ++i) {
        eClass[i] = s[i];
    }
    
    auto sortableEnd = sortable.begin() + s.size();
    
    for (int iter = 0, iter_total = log2(s.size()) + 2; iter < iter_total; ++iter) {
        std::iota(sortable.begin(), sortableEnd, 0);
        
        std::sort(sortable.begin(), sortableEnd, [iter, &s](int posA, int posB) {
            if ( eClass[posA] != eClass[posB] ) { return eClass[posA] < eClass[posB]; }
            int posAmid = (posA + (1 << iter)) % s.size();
            int posBmid = (posB + (1 << iter)) % s.size();
            return eClass[posAmid] < eClass[posBmid];
        });

        auto equal = [iter, &s](int posA, int posB) -> bool {
            int posAmid = (posA + (1 << iter)) % s.size();
            int posBmid = (posB + (1 << iter)) % s.size();
            return (eClass[posA] == eClass[posB]) && (eClass[posAmid] == eClass[posBmid]);
        };
        
        eClassNext[sortable[0]] = 0;
        for (int i = 1, _class = 0; i < s.size(); i++) {
            if (!equal(sortable[i - 1], sortable[i])) ++_class;
            eClassNext[sortable[i]] = _class;
        }

        std::copy(eClassNext.begin(), eClassNext.end(), eClass.begin());
    }

#ifdef DEBUG
    auto cyclic_substr = [](std::string s, int i) -> std::string { return s.substr(i, s.size() - i) + s.substr(0, i); };

    std::cout << "--- Built suffix array ---" << std::endl;
    
    std::cout << "  For string: " << s << std::endl; 
    std::for_each(sortable.begin() + 1, sortableEnd, [&s, &cyclic_substr](int i){ std::cout << "  | " << cyclic_substr(s, i) << std::endl; });
    std::cout << std::endl;
    
    std::cout << "  Suffix Array and Inverse Suffix Array: " << std::endl;

    std::vector<int> inverse(s.size() - 1);
    for (int i = 0; i < s.size() - 1; ++i) {
        inverse[*(sortable.begin() + 1 + i)] = i;
    }

    for (int i = 0; i < s.size() - 1; ++i) {
        std::cout << "  | [ " << i << " ]\t" << *(sortable.begin() + 1 + i) << "\t" << inverse[i] << std::endl;
    }

    std::cout << "--------------------------" << std::endl;
#endif
    // UNIQ them
    writeIdx = 0;
    eClassNext[0] = sortable[0];
    for (int i = 1; i < s.size() - 1; i++) {
        if (eClass[sortable[i]] != eClass[sortable[i - 1]]) {
            eClassNext[++writeIdx] = sortable[i];
        }
    }
     
    return eClassNext.begin();
}
}

int main() {
    auto cyclic_substr = [](std::string s, int i) -> std::string { return s.substr(i, s.size() - i) + s.substr(0, i); };

    std::ifstream in("shifts.in");
    std::ofstream out("shifts.out");
    //std::istream& in = std::cin;
    //std::ostream& out = std::cout;
    
    std::string s;
    //in >> s;
    std::getline(in, s);

    auto it = SufArrayAndCo::buildSufArray(s);
    /*std::cout << writeIdx << std::endl; 
    for (int i = 0; i <= writeIdx; i++) {
        std::cout << s.substr(*(it + i)) << std::endl;
    }*/
    
    int i;
    in >> i;
    --i;

    if (i > writeIdx) {
        out << "IMPOSSIBLE" << std::endl;
        return 0;
    }

    out << cyclic_substr(s, *(it + i)) << std::endl;
    return 0;
}
