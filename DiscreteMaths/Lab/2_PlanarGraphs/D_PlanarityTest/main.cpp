#include <iostream>
#include <vector>
#include <array>
#include <string>
#include <map>
int n;
std::array<std::array<bool, 6>, 6> matrix;
std::map<int, int> sizeToN;

void read() {
    std::string s;
    std::getline(std::cin, s);
    //std::cin >> s;
    n = sizeToN[s.length()];
    int t = 0;
    std::fill(matrix.begin(), matrix.end(), std::array<bool, 6>({0,0,0,0,0,0}));
    for(int i = 0; i < n; i++) {
        matrix[i][i] = 0;
        for(int j = 0; j < i; j++) {
            matrix[j][i] = matrix[i][j] = s[t++] == '1';
        }
    }
}
std::array<std::array<bool, 5>, 5> delVert(int a, const std::array<std::array<bool, 6>, 6>& m) {
    std::array<std::array<bool, 5>, 5> ret;
    /*for(int i = 0; i < a; i++) {
        for(int j = 0; j < a; j++) {
            ret[i][j] = m[i][j];
        }
        for(int j = a + 1; j < 6; j++) {
            ret[i][j - 1] = m[i][j];
        }
    }
    for(int i = a + 1; i < 6; i++) {
        for(int j = 0; j < a; j++) {
            ret[i-1][j] = m[i-1][j];
        }
        for(int j = a + 1; j < 6; j++) {
            ret[i-1][j - 1] = m[i-1][j];
        }
    }*/
    for(int i = 0; i < 5; i++) {
        for(int j = 0; j < 5; j++) {
            ret[i][j] = m[(i >= a) ? i + 1 : i][(j >= a ? j + 1 : j)];
        }
    }

    return ret;
}
std::array<std::array<bool, 5>, 5> delEdge(int a, int b) {
    auto m = matrix;

    for(int i = 0; i < 6; i++) {
        for(int j = 0; j < 6; j++) {
            m[a][i] |= m[b][i];
            m[i][a] |= m[i][b];
        }
    }

    a = b;

    return delVert(a, m);
}

bool chk_K5(std::array<std::array<bool, 5>, 5> in) {
    for(int i = 0; i < 5; i++)
        for(int j = 0; j < 5; j++)
            if(i != j && in[i][j] == 0) return false;
    return true;
}

bool find_K5() {
    if(n < 5) return false;
    for(int i = 0; i < 6; i++) {
        for(int j = 0; j < 6; j++)
        if(matrix[i][j] && j != i && chk_K5(delEdge(i,j))) {
            return true;
        }
        auto p = delVert(i, matrix);
        if(chk_K5(delVert(i, matrix))) {
            return true;
        }
    }
    return false;
}

bool chk_K33(int a, int b, int c) {
    int cnt = 0;
    for(int i = 0; i < n; i++) {
        for(int j = 0; j < n; j++) {
            if((i == a || i == b || i == c) && !(j == a || j == b || j == c)){
                cnt += matrix[i][j];
            }
        }
    }

    return cnt == 9;
}

bool find_K33() {
    if(n < 6) return false;
    for(int i = 0; i < n; i++) {
        for(int j = i + 1; j < n; j++) {
            for(int k = j + 1; k < n; k++) {
                if (chk_K33(i, j, k)) {
                    return true;
                }
            }
        }
    }
    return false;
}

int main()
{
    std::freopen("planaritycheck.in", "r", stdin);
    std::freopen("planaritycheck.out", "w", stdout);

    sizeToN[1] = 2;
    sizeToN[3] = 3;
    sizeToN[6] = 4;
    sizeToN[10] = 5;
    sizeToN[15] = 6;
    sizeToN[21] = 7;

    int KK;
    std::cin >> KK;
    std::string s;
    std::getline(std::cin, s);

    for(int i = 0; i < KK; i++) {
        read();
        //std::cout << "\n K5: " << find_K5() << " K33: " << find_K33() << std::endl;
        std::cout << ((find_K5() || find_K33()) ? "NO" : "YES") << std::endl;
    }

    return 0;
}
