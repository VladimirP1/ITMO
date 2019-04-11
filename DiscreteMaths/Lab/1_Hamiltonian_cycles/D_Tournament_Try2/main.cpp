#include <iostream>
#include <array>
#include <vector>
#include <algorithm>
#include <fstream>

#define __M_FILE

#ifdef __M_FILE
std::ifstream in("guyaury.in");
std::ofstream out("guyaury.out");
#else
std::istream& in = std::cin;
std::ostream& out = std::cout;
#endif

bool getNextZO();

int size = 0;
std::array<std::array<bool, 1001>, 1001> dir;


void read() {
    in >> size;
    for(int i = 0; i < size; i++) {
        dir[i][i] = 0;
        for(int j = 0; j < i; j++) {
           dir[j][i] = !(dir[i][j] = getNextZO());
        }
    }
}

std::vector<int> findHamiltionianPath() {
    std::vector<int> lamps;

    for(int i = 0; i < size ; i ++) {
        auto ins = std::upper_bound(lamps.begin(), lamps.end(), i, [](int a, int b){
            return dir[a][b];
        });
        lamps.insert(ins, i);
    }

    for(int i = 1; i < size; i++) {
        if (!dir[lamps[i - 1]][lamps[i]]) {
            exit(1);
        }
    }

    return lamps;
}



int main()
{
    read();
    auto p = findHamiltionianPath();

    //for_each(p.begin(), p.end(), [](int i){ std::cout << i << std::endl; });

    std::vector<int> cyc;
    int i;

    for(i = p.size() - 1; i >= 2; i--) {
        if(dir[p[i]][p[0]]) {
            break;
        }
    }

    cyc.insert(cyc.begin(), p.begin(), p.begin() + i + 1);
    p.erase(p.begin(), p.begin() + i + 1);

    for(auto outCyc = p.begin();outCyc != p.end();) {
        auto inCyc = cyc.begin();
        while(inCyc != cyc.end() && !dir[*outCyc][*inCyc]) {
            inCyc++;
        }
        if(inCyc != cyc.end()) {
            cyc.insert(inCyc, p.begin(), outCyc + 1);
            p.erase(p.begin(), outCyc + 1);
            outCyc = p.begin();
        } else {
            outCyc++;
        }
    }

    for_each(cyc.begin(), cyc.end(), [](int i){ out << (i + 1) << std::endl; });

    return 0;
}


bool getNextZO() {
    char c = 0;
    while ((c != '0') && (c != '1')) {
        in.get(c);
    }
    return c == '1';
}
