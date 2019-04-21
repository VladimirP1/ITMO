#pragma GCC target("avx")
#pragma GCC optimize("Ofast")
#pragma GCC optimize("inline")
#include <ext/pb_ds/assoc_container.hpp>
#include <ext/pb_ds/tree_policy.hpp>
#include <iostream>

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    
    int n;
    __gnu_pbds::tree<int, __gnu_pbds::null_type, std::greater<int>, __gnu_pbds::rb_tree_tag, __gnu_pbds::tree_order_statistics_node_update>  s;
    
    std::cin >> n;
    
    for (int i = n, op, arg; i--; ) {
        std::cin >> op >> arg;
        if (op == 1) {
            s.insert(arg);
        } else if (op == 0) {
            std::cout << *s.find_by_order(arg - 1) << "\n";
        } else if (op == -1) {
            s.erase(arg);
        }
    }

    std::cout << std::flush;
    
    return 0;
}
