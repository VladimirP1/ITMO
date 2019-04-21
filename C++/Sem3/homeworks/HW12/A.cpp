#pragma GCC optimize ("O3")
#include <iostream>
#include <set>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);

    int x;
    std::set<int> s;

    for (std::string cmd; (std::cin >> cmd).good(); ) {
        std::cin >> x;
        switch(cmd[0]) {
            case 'i': {
                s.insert(x); 
            } break;

            case 'd': {
                s.erase(x);
            } break;

            case 'e' : {
                std::cout << ((s.find(x) != s.end()) ? "true\n" : "false\n");
            } break;

            case 'n' : {
                auto result = s.upper_bound(x);
                
                if(result != s.end()) {
                    std::cout << *result << "\n"; 
                } else {
                    std::cout << "none\n";  
                }
            } break;

            case 'p' : {
                auto result = s.lower_bound(x);
                
                if(result != s.begin()) {
                    std::cout << *--result << "\n"; 
                } else {
                    std::cout << "none\n";  
                }
            } break;
        }
    }
    std::cout << std::flush;

}
