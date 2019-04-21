#include <algorithm>
#include <iostream>
#include <vector>
#include <array>
#include <queue>

using namespace std;

using _dtype = int64_t;
using _dvpair = std::pair<_dtype, int64_t>;
int64_t n, m;

std::array<_dtype, 5004> d;
std::array<std::array<_dtype,5004>,5004> matrix; 

const _dtype INF = std::numeric_limits<_dtype>::max();

void dijkstra();

struct HeavyObject {
    _dtype X1, X2, Y1, Y2;

	HeavyObject(int64_t X1, int64_t X2, int64_t Y1, int64_t Y2) :
        X1(X1), X2(X2), Y1(Y1), Y2(Y2) {
    }

    HeavyObject(std::istream& in) {
        in >> X1 >> Y1 >> X2 >> Y2;
    }

    bool intersect(int64_t a1, int64_t a2, int64_t b1, int64_t b2) {
        if (b1 <= a1 && a1 <= b2) return true;
        if (b1 <= a2 && a2 <= b2) return true;
        if (a1 <= b2 && b2 <= a2) return true;
        if (a1 <= b2 && b2 <= a2) return true;

        return false;
    }

    int64_t dist(int64_t a1, int64_t a2, int64_t b1, int64_t b2) {
        if (a1 > b1) {
            std::swap(a1,b1);
            std::swap(a2,b2);
        }

        if (intersect(a1,a2,b1,b2)) {
            return 0;
        }
        
        return b1 - a2;
    }

    int64_t operator-(const HeavyObject& b) {
        return std::max(dist(X1, X2,b.X1,b.X2),dist(Y1,Y2,b.Y1,b.Y2));
    }
};


struct Solver {
    int64_t N, W;
    std::vector<HeavyObject*> objects;

    void read() {
        std::cin >> N >> W;

        objects.push_back(new HeavyObject(std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max(), 0, 0));
        objects.push_back(new HeavyObject(std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max(), W, W));

        for(int64_t i = 0; i < N; i++) {
            objects.push_back(new HeavyObject(std::cin));
        }

        for(int64_t i = 0; i < objects.size(); i++) {
            for(int64_t j = 0; j < objects.size(); j++) {
                _dtype d = *objects[i] - *objects[j];
                //std::cout << "dist " << i << " " << j << " " << d << std::endl;
                matrix[i][j] = d;
            }
        }

        n = objects.size();
        m = n*n;

        std::fill(d.begin(), d.end(), std::numeric_limits<_dtype>::max() / 2);
    }

    _dtype solve() {
        dijkstra();
        return d[1];
    }
};

void dijkstra() {
    std::priority_queue<_dvpair, std::vector<_dvpair>, std::greater<_dvpair>> q;
    q.push({0, 0});
    d[0] = 0;
    for(int64_t i = 0; i < n; i++) {
        _dvpair top;
        do {
           top = q.top();
           q.pop();
        } while(top.first != d[top.second]);

        auto vertex = top.second;
        for(int64_t ee = 0; ee < n; ++ee) {
            _dvpair e = {matrix[vertex][ee], ee};
            _dtype m = min(d[e.second], d[vertex] + e.first);
            if (d[e.second] > m) {
                d[e.second] = m;
                q.push({d[e.second], e.second});
            }
        }
    }
}

int main()
{
    Solver s;
    s.read();
    std::cout << s.solve() << std::endl;
    return 0;
}
