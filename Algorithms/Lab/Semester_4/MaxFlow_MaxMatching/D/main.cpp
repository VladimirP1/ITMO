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

    
    static void mkEdges(int a, int b, FlowType capacity, Edge*& direct, Edge*& reverse) {
        direct =  new Edge(a, b, capacity, false);
        reverse = new Edge(b, a, 0       , true );
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
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->inResidualNet() && e->isInLayeredNet;}    );
    }
    FilterWrapper<std::vector<Edge*>> getNzfIterable() {
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->flow > 0;});
    }

};

struct Graph {
    int Na, Nb;
    int N, M;
    int S, T;
    std::vector<Vertex> verticies;
    std::vector<Edge*> outputOrder;
       
    static const decltype(verticies.back().distance) distInf = 
        std::numeric_limits<decltype(verticies.back().distance)>::max();

    char get_v_type(int v) {
        if (v < Na) return 'A';
        if (v < Na + Nb) return 'B';
        return 'C';
    }

    void read(std::istream& in) {
        {
            in >> Na >> Nb;
            N = Na + Nb;

            verticies.resize(N);
        }
        {
            int from, to;

            std::string line_;
            std::stringstream line;

            std::getline(in, line_); 
            
            for(int i = 0, a ,b; i < Na; i++) {
                line.clear();
                std::getline(in, line_);
                line.str(line_);
                
                from = i;

                while(1) {
                    line >> to;
                    if (to == 0) break;
                    to += Na - 1;

                    Edge *direct, *reverse;
                    Edge::mkEdges(from, to, 1, direct, reverse);

                    verticies[from].edges.push_back(direct);
                    verticies[to].edges.push_back(reverse);

                    outputOrder.push_back(direct);
                }

            }
        }
        {
            N += 2;
            verticies.resize(N);
            
            S = N - 2;
            T = N - 1;

            const auto& mkEdge = [&](int from, int to){
                Edge *direct, *reverse;
                Edge::mkEdges(from, to, 1, direct, reverse);

                verticies[from].edges.push_back(direct);
                verticies[to].edges.push_back(reverse);

                outputOrder.push_back(direct);
            };
           
            for (int i = 0; i < Na; i++) {
                mkEdge(S, i);
            }
            
            for (int i = 0; i < Nb; i++) {
                mkEdge(Na + i, T);
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

        //std::cout << g.verticies[g.T].distance << std::endl; 
        //std::cout << "----" << std::endl;
        //for(auto v : g.verticies) {
        //    std::cout << v.distance << " " << v.parent << std::endl;
        //    for(auto e : v.edges) {
        //        std::cout << "    "  << e->from << " " << e-> to << " " << e->capacity << std::endl;
        //    }
        //}
        if (g.verticies[g.T].distance == g.distInf) break;

        g.layeredNetDFS();
    }

    std::cout << g.getFlow() << std::endl;

    for(auto e : g.outputOrder) {
        if(g.get_v_type(e->from) == 'A' && g.get_v_type(e->to) == 'B' && e->flow == 1) {
            std::cout << e->from + 1 << " " << e->to + 1 - g.Na << std::endl;
        } 
    }

    return 0;
}
