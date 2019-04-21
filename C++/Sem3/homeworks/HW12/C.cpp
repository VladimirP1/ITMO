#pragma GCC optimize ("O3")
#include <iostream>
#include <queue>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    
    int n, x;
    char c;
    std::queue<int> q;

    std::cin >> n;
    
    for (int i = 0; i < n; ++i) {
        if(std::istream::sentry(std::cin)) {
            std::cin >> c;
            switch (c) {
                case '+':
                    std::cin >> x;
                    q.push(x);
                    break;
                
                case '-':
                    std::cout << q.front() << '\n';
                    q.pop();
                    break;
            }
        }
    }

    std::cout << std::flush;
        
}
