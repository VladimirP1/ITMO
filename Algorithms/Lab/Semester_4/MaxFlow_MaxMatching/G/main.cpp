#include <vector>
#include <iostream>
#include <numeric>
#include <algorithm>
#include <map>
#include <sstream>
#include <set>
#include <assert.h>
#include <math.h>
#include <iterator>
#include <functional>

void dec() {}

template <typename T, typename... Args>
void dec(T& a, Args&... args) {
    --a;
    dec(args...);
}

using FlowType = int;

template<class T>
class FilterWrapper {
public:
    using I = typename T::iterator;
    class iterator : public std::iterator<std::input_iterator_tag, typename std::iterator_traits<I>::value_type> {
    public:
        using PT = std::function<bool (const typename std::iterator_traits<iterator>::value_type &)>;
        explicit iterator(
                    I begin,
                    I end,
                    PT predicate
                ) 
                    : begin_(begin), end_(end), predicate_(predicate) { skip(); }
        iterator& operator++() { 
            ++begin_;
            skip();
            return *this;
        }
        bool operator==(const iterator& other) const { return begin_ == other.begin_; }
        bool operator!=(const iterator& other) const { return !(*this == other); }
        typename std::iterator_traits<iterator>::reference operator*() const { return *begin_; }
    private:    
        I begin_;
        I end_;
        PT predicate_;
        
        void skip() {
            for(; begin_ != end_; ++begin_) 
                if (predicate_(*begin_)) break;
        }
    };

    FilterWrapper(
            typename T::iterator begin,
            typename T::iterator end,
            typename iterator::PT predicate) : 
                begin_(iterator(begin, end, predicate)), end_(iterator(end, end, predicate)) {}

    iterator begin() {return begin_;}
    iterator end() {return end_;}
private:
    iterator begin_;
    iterator end_;
};

struct Edge {
    int from, to;
    Edge* complement;
    FlowType capacity;
    FlowType flow = 0;
    
    bool isReverse;
    bool isInLayeredNet;

    char type = 0;
    
    static void mkEdges(int a, int b, FlowType capD, FlowType capR, Edge*& direct, Edge*& reverse) {
        direct =  new Edge(a, b, capD, false);
        reverse = new Edge(b, a, capR, true );
        direct->complement = reverse;
        reverse->complement = direct;
    }

    void addFlow(FlowType dFlow) {
        capacity -= dFlow;
        complement->capacity += dFlow;
        flow += dFlow;
        complement->flow -= dFlow;
    }

    bool inResidualNet() const {
        return capacity > 0;
    }

    Edge() {} // for std::vector
private:
    Edge (int from, int to, FlowType capacity, bool isReverse) : from(from), to(to), capacity(capacity), isReverse(isReverse) {}
};

struct Vertex {
    std::vector<Edge*> edges;
    int distance;
    Edge* parent;

    FilterWrapper<std::vector<Edge*>> getResidualIterable() {
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->inResidualNet();});
    }
    FilterWrapper<std::vector<Edge*>> getLayeredIterable() {
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->inResidualNet() && e->isInLayeredNet;});
    }
    FilterWrapper<std::vector<Edge*>> getNzfIterable() {
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->flow > 0;});
    }

};

struct Graph {
    int N;
    int n, m;
    int S, T;
    std::vector<Vertex> verticies;
    std::vector<Edge*> outputOrder;
    std::array<std::array<char, 50>, 50> matrix;
       
    static const decltype(verticies.back().distance) distInf = 
        std::numeric_limits<decltype(verticies.back().distance)>::max();

    auto linearize (int i, int j) {
        return n * i + j;
    }

    void read(std::istream& in) {

        const auto& remember_edge = [&](Edge* e) {
            outputOrder.push_back(e);
        };
        const auto& mk_v_edge = [&](int from, int to, FlowType capD, FlowType capR) {
            Edge *direct, *reverse;
            Edge::mkEdges(from, to, capD, capR, direct, reverse);
            verticies[from].edges.push_back(direct);
            verticies[to].edges.push_back(reverse);
            remember_edge(direct);
            return direct;
        };
        {
            std::string line;
            std::cin >> m >> n;


            std::getline(in, line); // Eat leftover \n

            for(int i = 0; i < m; ++i) {
                std::getline(in, line);
                for(int j = 0; j < n; ++j) {
                    char c = line[j];
                    matrix[i][j] = c;
                }
            }

            N = 2 * n * m;

            verticies.resize(N);

            // Add limiting edges
            for(int i = 0; i < m; i++) {
                for(int j = 0; j < n; j++) {
                    if (matrix[i][j] == '#') {
                        // Do nothing
                    } else if (matrix[i][j] == '.') {
                        mk_v_edge(linearize(i, j), n*m + linearize(i, j), 1, 0)->type = '.';
                    } else if (matrix[i][j] == '-') {
                        mk_v_edge(linearize(i, j), n*m + linearize(i, j), 100*100, 0)->type = '-';
                    } else if(matrix[i][j] == 'A') {
                        S = linearize(i, j) + m*n;
                    } else if(matrix[i][j] == 'B') {
                        T = linearize(i, j);
                    }
                }
            }

            // Add walking edges
            for(int i = 0; i < m; i++) {
                for(int j = 0; j < n; j++) {
                    if(j + 1 < n)  {
                        mk_v_edge(m*n + linearize(i    , j + 1), linearize(i, j), 100*100, 0);
                    }
                    if(i + 1 < m)  {
                        mk_v_edge(m*n + linearize(i + 1, j    ), linearize(i, j), 100*100, 0);
                    }
                    if(j - 1 >= 0) {
                        mk_v_edge(m*n + linearize(i    , j - 1), linearize(i, j), 100*100, 0);
                    }
                    if(i - 1 >= 0) {
                        mk_v_edge(m*n + linearize(i - 1, j    ), linearize(i, j), 100*100, 0);
                    }
                }
            }
        }
    }
    
    void makeLayeredNetwork() {
        
        std::for_each(verticies.begin(), verticies.end(), [&](Vertex& v){ 
                v.distance = distInf;
        });
        {
            verticies[S].distance = 0;
            std::set<std::pair<int, int>> q {{S,0}};
            while (!q.empty()) {
                auto top = [&](){ 
                        auto x = q.begin();
                        auto y = *x;
                        q.erase(x);
                        return y;
                    }();
                for (auto e : verticies[top.first].getResidualIterable()) {
                    if ( verticies[e->to].distance == distInf ) {
                        verticies[e->to].distance = verticies[top.first].distance + 1;
                        q.insert({e->to, verticies[e->to].distance});
                    }
                }
            }
            for (auto e : outputOrder) {
                const auto& run = [&](Edge* e){
                    e->isInLayeredNet = verticies[e->from].distance + 1 == verticies[e->to].distance;
                };
                run(e);
                run(e->complement);
            }
        }
    }

    void layeredNetDFS() {
        std::for_each(verticies.begin(), verticies.end(), [](Vertex& v){ 
                v.parent = (Edge*) 0;
        });
        verticies[S].parent = (Edge*) 1;
        std::function<void(int)> myDFS = [&](int v) {
            for (auto e : verticies[v].getLayeredIterable()) {
                if (!verticies[e->to].parent) {
                    verticies[e->to].parent = e;
                    myDFS(e->to);
                }
            }
        };
        while(1) {
            myDFS(S);

            if (!verticies[T].parent) break;
            
            FlowType minCap = std::numeric_limits<FlowType>::max();  
            for( auto x = verticies[T].parent;
                 x != (Edge*) 1;
                 x = verticies[x->from].parent) 
                        minCap = std::min(minCap, x->capacity);

            if (minCap == 0) break;
            
            for( auto x = verticies[T].parent;
                 x != (Edge*) 1;
                 x = verticies[x->from].parent) 
                        x->addFlow(minCap);
        }
    }

    std::vector<Edge*> findMinCut() {

        std::vector<Edge*> inCut;
        for (int i = 0; i < outputOrder.size(); i++) {
            Edge* e = outputOrder[i];
            const auto& check = [&](Edge* e){
                if (verticies[e->from].distance != distInf &&
                        verticies[e->to].distance == distInf) {
                    inCut.push_back(e);
                }
            };
            check(e);
        }
        return inCut;
    }

    void printMap() {
        {
            const auto& decode = [&](int x){ return std::pair<int,int>(x / n, x % n); };
            auto minCut = findMinCut();
            for (auto pe : minCut) {
                auto pos = decode(pe->from);
                matrix[pos.first][pos.second] = '+';
            }
        }
        {
            for(int i = 0; i < m; i++) {
                for(int j = 0; j < n; j++) {
                    std::cout << matrix[i][j];
                }
                std::cout << std::endl;
            }
        }
    }

    FlowType getFlow() {
        FlowType flow = 0;
        for (auto e : verticies[S].edges) {
            flow += e->flow;
        }
        return flow;
    }
};


int main() {
    Graph g;
    g.read(std::cin);

    while(1) {
        g.makeLayeredNetwork();


        if (g.verticies[g.T].distance == g.distInf) break;

        g.layeredNetDFS();
    }

    FlowType f = g.getFlow();

    if (f >= 100*100) {
        std::cout << -1 << std::endl;
        return 0;
    }

    std::cout << g.getFlow() << std::endl;

    assert(g.getFlow() == g.findMinCut().size());

    g.printMap();

    return 0;
}
