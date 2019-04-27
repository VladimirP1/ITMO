#include <iostream>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <fstream>

std::ifstream in("assignment.in");
std::ofstream out("assignment.out");

using cost_t = int;
static constexpr cost_t COST_MAX = std::numeric_limits<cost_t>::max();

size_t n;
std::vector<std::vector<cost_t>> cost;

int main()
{
    in >> n;
    cost.resize(n + 1);

    for(int i = 1; i < n + 1; ++i) {
        cost[i].resize(n + 1);
        for (int j = 1; j < n + 1; ++j) {
            in >> cost[i][j];
        }
    }

    std::vector<int> u(n + 1), v(n + 1);
    std::vector<int> match(n + 1);
    std::vector<int> chain(n + 1);

    int cur_col;

    for (int i = 1; i < n + 1; ++i) {
        std::vector<int> min_in_col(n + 1, COST_MAX);
        std::vector<uint8_t> used_cols(n + 1, 0);
        cur_col = 0;
        match[0] = i;

        do {
            int min_col;
            int min_val = COST_MAX;
            used_cols[cur_col] = true;

            { // Find min_val and min_col (in visited rows, unvisited cols)
                int cur_row = match[cur_col];
                for(int j = 1; j < n + 1; ++j) {
                    if (!used_cols[j]) { // not in Z_2
                        int cell = cost[cur_row][j] - u[cur_row] - v[j];
                        if (cell < min_in_col[j]) {
                            min_in_col[j] = cell;
                            chain[j] = cur_col;
                        }
                        if(min_in_col[j] < min_val) {
                            min_val = min_in_col[j];
                            min_col = j;
                        }
                    }
                }
            }

            { // Apply +delta -delta
                for (int j = 0; j < n + 1; ++j) {
                    if (used_cols[j]) {
                        u[match[j]] += min_val;
                        v[j] -= min_val;
                    } else {
                        min_in_col[j] -= min_val;
                    }
                }
            }

            cur_col = min_col; // continue search of aug. path
        } while (match[cur_col] != 0);

        do { // Apply changes along aug. path
            int next_col = chain[cur_col];
            match[cur_col] = match[next_col];
            cur_col = next_col;
        } while(cur_col);
    }

    int ans = 0;
    for(int i = 1; i < n + 1; ++i) {
        ans += cost[match[i]][i];
    }
    out << ans << std::endl;

    for (int i = 1; i < n + 1; ++i) {
        if(match[i]) {
            out << match[i] << " " << i << std::endl;
        }
    }



    return 0;
}
