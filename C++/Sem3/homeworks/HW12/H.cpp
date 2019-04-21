#pragma GCC optimize ("O3")
#include <iostream>
#include <exception>
#include <unordered_set>

int x;
char buf[20];
std::unordered_set<int> s;

bool run() {

    std::cin.getline(buf, 20);
   
    if (std::cin.eof()) {
        return false;
    }

    int x = atoi(&buf[7]);
    
    switch (buf[0]) {
        case 'i': {
            s.insert(x);
            break;
        }
        case 'e': {
            std::cout << (s.find(x) == s.end() ? "false" : "true") << "\n";
            break;
        }
        case 'd': {
            s.erase(x);
            break;
        }
    }
     

    return true;
}

int main() {
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);

    try {
        while (run());
    } catch(std::exception&) {

    }
    
    std::cout << std::flush;

    return 0;
}
