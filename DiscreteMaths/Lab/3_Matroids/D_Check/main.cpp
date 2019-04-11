#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <math.h>

std::ifstream in("check.in");
std::ofstream out("check.out");

int lX, lS;
std::vector<int> S;

inline bool isAsubsetB(int a, int b) {
    return (a & ~b) == 0;
}

inline int csb(unsigned int n)
{
  int count = 0;  while (n)  {   count += n & 1;  n >>= 1;  }  return count;
}

int genNones(int n) {
    int ret = 0;
    for(int i = 0; i < n; i++) {
        ret |= 1;
        ret <<= 1;
    }
    return ret;
}

bool comp_by_bits(int a, int b) {
    return csb(a) < csb(b);
}

void read() {
    int l = 0;
    in >> lX >> lS;
    for(int i = 0; i < lS; i++) {
        in >> l;
        int smask = 0;
        for(int i = 0; i < l; i++) {
            int tmp;
            in >> tmp;
            --tmp;
            smask |= (1 << tmp);
        }
        S.push_back(smask);
    }
    std::sort(S.begin(), S.end(), comp_by_bits);
}

bool chk_1() {
    if(S.empty()) return false;
    return S[0] == 0;
}

bool chk_2() {
    for(auto i : S) {
        int targetCount = pow(2, csb(i));
        for(auto j : S) {
            if(isAsubsetB(j, i)) {
                --targetCount;
            }
        }
        if(targetCount) {
            return false;
        }
    }
    return true;
}

bool chk_3(){
    for(int i = 0; i < S.size(); i++) {
        for(int j = 0; j < i; j++) {
            int A = S[j];
            int B = S[i];
            if(csb(A) == csb(B)) continue;
            // |A| < |B| now
            // A, B in I
            // should exisit x in B\A such than A U x in I
            int BminusA = B & ~A;
            auto start = std::lower_bound(S.begin(), S.end(), genNones(csb(A) + 1), comp_by_bits);
            auto stop = std::upper_bound(S.begin(), S.end(), genNones(csb(A) + 1), comp_by_bits);
            //std::cout << csb(*start) << std::endl;
            bool found = 0;
            //if(start > stop) return false;
            for(; start != stop; ++start) {
                //std::cout << csb(A^(*start)) << std::endl;
                if((csb(A^(*start)) == 1 && ((A^(*start)) & BminusA))) found = 1;
            }
            //std::cout << "---" << std::endl;
            if(!found) {
                //std::cout << A << " " << B << std::endl;
                return false;
            }
        }
    }
    return true;
}

int main()
{
    read();
    if(!chk_1()) goto fail;
    if(!chk_2()) goto fail;
    if(!chk_3()) goto fail;
    out << "YES" << std::endl;
    return 0;
fail:
    out << "NO" << std::endl;
    return 0;
}
