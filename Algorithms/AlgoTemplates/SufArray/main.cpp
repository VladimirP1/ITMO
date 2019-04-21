#include <cassert>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <array>
#include <string>
#include <numeric>
#include <functional>

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
    s += "$";
    
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
    std::cout << "--- Built suffix array ---" << std::endl;
    
    std::cout << "  For string: " << s << std::endl; 
    std::for_each(sortable.begin() + 1, sortableEnd, [&s](int i){ std::cout << "  | " << s.substr(i) << std::endl; });
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

    return sortable.begin() + 1;
}

std::array<int, N> inverseSuffixArray;
std::array<int, N> lcpArray;

template<class Iterator>
returned_iterator_type buildLCP(const std::string& s, Iterator suffixIterator) {
    int n = s.size();

    auto suffixArray = [&suffixIterator](size_t x) -> int& { return *(suffixIterator + x); };

    for (int i = 0; i < n; i++) {
        inverseSuffixArray[suffixArray(i)] = i;
    }
    
    std::fill(lcpArray.begin(), lcpArray.end(), -1);
    
    int k = 0;
    for (int z = 0; z < n; z++) {
        int i = inverseSuffixArray[z];
       
        if ( i == 0 ) { k = 0; continue; }

        k = ([&](int start) { 
            for(int j = start; 1; j++) {
                if(s[j + suffixArray(i - 1)] != s[j + suffixArray(i)]) {
                    return j;
                }
            }
        })(k);
        
        lcpArray[i] = k;

        //std::cout << "lcp[" << i << "] = " << k << std::endl;
        
        if (k) --k;
    }
    return lcpArray.begin() + 1;
}
};

int main() {
    std::ifstream in("array.in");
    std::ofstream out("array.out");
    //std::istream& in = std::cin;
    //std::ostream& out = std::cout;
    
    std::string s;
    in >> s;
    auto it = SufArrayAndCo::buildSufArray(s);
    auto it2 = SufArrayAndCo::buildLCP(s, it);
    
    for (int i = 0; i < s.size(); i++) {
        out << (*it) + 1 << " ";
        ++it;
    }
    out << std::endl;

    for (int i = 1; i < s.size(); i++) {
        out << (*it2) << " ";
        ++it2;
    }
    out << std::endl;
    
}
