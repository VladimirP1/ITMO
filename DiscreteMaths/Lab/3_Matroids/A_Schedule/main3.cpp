#include <algorithm>
#include <iostream>
#include <fstream>
#include <limits>
#include <set>
#include <map>
#include <vector>
const int INF = std::numeric_limits<int>::max();

class task {
public:
    task() : fine(INF), deadline(INF) {}
    task (int fine, int deadline) : fine(fine), deadline(deadline) {}
    int f (int time) const { return (time >= deadline) ? fine : 0; }
    bool operator<(const task& b) const { 
        return fine < b.fine; 
    }
    static task read(std::istream& in) {int d, w; in >> d >> w; return task(w, d);}

    int fine;
    int deadline;
};

int n;
int maxDL = 0;
std::vector<task> tasks;

void read(std::istream& in) {
    in >> n;
    for (int i = 0; i < n; i++) {
        auto t = task::read(in);
        maxDL = std::max(maxDL, t.deadline);
        tasks.push_back(t);
    }
    maxDL += 2;
}

uint64_t schedule() {
    std::map<int, task> table;
    table.insert({-1, task{-1, INF}});
    table.insert({maxDL, task{maxDL, INF}});
    int n = tasks.size();
    int used_bound = -1;
    std::sort(tasks.begin(), tasks.end());
    std::reverse(tasks.begin(), tasks.end());
    for (auto t : tasks) {
        auto dead_task = table.lower_bound(t.deadline);
        // case 1: slot a deadline - 1 is free
        int kk = -1;
        if (table.find(t.deadline - 1) == table.end()) {
            kk = t.deadline - 1;
        } else {
            --dead_task; // dead_task now points to t.deadline-1
            int zz = t.deadline - 1;
            while(zz) {
                --zz;
                --dead_task;
                if(dead_task->first != zz) {
                    kk = zz;
                    break;
                }
                if(dead_task == table.begin()) {
                    break;
                }
                if(dead_task->first < used_bound) break;
            }
        }
        if(kk < 0) {
            used_bound = std::max(used_bound, t.deadline - 1);
        }
        /*if (table.begin() == dead_task) {
            table[t.deadline - 1] = t;
            continue;
        }
        
        int kk = -1;
        while(dead_task != table.begin()) {
            int probepos = std::min(dead_task->first - 1, t.deadline - 1);
            //if (probepos - (--dead_task)->first > 1) {
            if (table.find(probepos) == table.end()) {
                kk = probepos;
                break;
            }
            --dead_task;
        }*/

        auto k = kk;
        if (k >= 0) {
            table[k] = t;
            //std::cout << k << " - " << kk << " : " << t.deadline << std::endl;
            //std::cout << "Found a gap at j = " << k << std::endl; 
            continue;
        }

        
        
        dead_task = table.lower_bound(t.deadline);
        auto min_task = min_element(table.begin(), dead_task);
        if(std::distance(table.begin(), min_task) == 0) {
            table[++maxDL] = t;
            continue;
        }
        //std::cout << "mt " << min_task -> first << std::endl;
        if (min_task->second.fine >= t.fine) {
            table[++maxDL] = t;
        } else {
            table[++maxDL] = min_task->second;
            min_task->second = t;
        }
    }
    uint64_t fine_sum = 0;
    for(auto x : table) {
        //std::cout << x.first << " (" << x.second.deadline << ", " << x.second.fine << " " << std::endl;
        fine_sum += x.second.f(x.first);
    }
    //std::cout << "Result = " << fine_sum << std::endl;
    return fine_sum;
}

int main()
{
    auto in = std::ifstream("schedule.in");
    auto out = std::ofstream("schedule.out");
    read(in);
    out << schedule() << std::endl;
    out.close();
    return 0;
}
