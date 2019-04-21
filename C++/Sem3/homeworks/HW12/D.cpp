#pragma GCC optimize ("O3")
#include <iostream>
#include <stack>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    
    int n, x;
    char c;
    std::stack<int> q;

    std::cin >> n;
    
    for (int i = 0; i < n; ++i) {
        std::cin >> c;
        switch (c) {
            case '+':
                std::cin >> x;
                q.push(x);
                break;
            
            case '-':
                std::cout << q.top() << '\n';
                q.pop();
                break;
        }
    }

    std::cout << std::flush;
        
}
