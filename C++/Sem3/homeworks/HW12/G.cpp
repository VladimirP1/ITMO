#pragma GCC optimize ("O3")
#include <vector>
#include <iostream>
#include <numeric>
#include <algorithm>

class Team {
public:
    Team() : idx(0) {}
    Team(int idx) : idx(idx) {}
    void read() {
        std::cin >> solved >> fine;
    }
    void print() const {
        std::cout << idx + 1 << ' ';
    }
    bool operator<(const Team& t) const {
        if (solved != t.solved) {
            return solved > t.solved;
        }
        if(fine != t.fine) {
            return fine < t.fine;
        }
        return idx < t.idx;
    }
private:
    int idx, solved, fine;
};

int main() {
    std::cout.tie(NULL);
    std::cin.tie(NULL);
    std::ios_base::sync_with_stdio(false);
    
    int n;
    std::cin >> n;
    std::vector<Team> teams;
    teams.resize(n);

    std::iota(teams.begin(), teams.end(), 0);
    std::for_each(teams.begin(), teams.end(), [](Team& t){t.read();});
    std::sort(teams.begin(), teams.end());
    std::for_each(teams.begin(), teams.end(), [](Team& t){t.print();});

    std::cout << std::endl;
}
