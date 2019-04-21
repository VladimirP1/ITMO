#pragma GCC optimize ("O3")
#include <iostream>
#include <unordered_map>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);

    std::string a, b, c;
    std::unordered_map<std::string, std::string> m;

    while ((std::cin >> a >> b).good()) {
        switch(a[0]) {
            case 'p': {
                std::cin >> c;
                m[b] = c;
            } break;
            
            case 'd': {
                m.erase(b);
            } break;
    
            case 'g': {
                auto x = m.find(b);
                
                if (x == m.end()) {
                    std::cout << "none\n";
                    break;
                }

                std::cout << x->second << "\n";
            } break;
        }
    }

    std::cout << std::flush;
}
