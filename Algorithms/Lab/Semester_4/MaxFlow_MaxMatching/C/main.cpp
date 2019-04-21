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
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->inResidualNet() && e->isInLayeredNet;});
    }
    FilterWrapper<std::vector<Edge*>> getNzfIterable() {
        return FilterWrapper<std::vector<Edge*>>(edges.begin(), edges.end(), [](Edge* const& e){ return e->flow > 0;});
    }

};

struct Graph {
    int N, M;
    int S, T;
    std::vector<Vertex> verticies;
    std::vector<Edge*> outputOrder;
    static const decltype(verticies.back().distance) distInf = 
        std::numeric_limits<decltype(verticies.back().distance)>::max();

    void read(std::istream& in) {
        in >> N >> M >> S >> T;
        dec(S, T);

        verticies.resize(N);

        for(int i = 0, a ,b; i < M; i++) {
            in >> a >> b;
            dec(a, b);

            Edge *direct, *reverse;
            Edge::mkEdges(a, b, 1, direct, reverse);

            verticies[a].edges.push_back(direct);
            verticies[b].edges.push_back(reverse);

            outputOrder.push_back(direct);
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
                        //std::cerr << "Use " << e->from << " " << e->to << std::endl;
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

            //std::cerr << "Can augment flow by " << minCap << std::endl;

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

    std::vector<int> printPath() {
        std::for_each(verticies.begin(), verticies.end(), [](Vertex& v){ 
                v.parent = (Edge*) 0;
        });
        verticies[S].parent = (Edge*) 1;

        std::function<void(int)> myDFS = [&](int v) {
            for (auto e : verticies[v].getNzfIterable()) {
                if (!verticies[e->to].parent) {
                    verticies[e->to].parent = e;
                    myDFS(e->to);
                }
            }
        };

        myDFS(S);
        std::vector<int> ans;
        for( auto x = verticies[T].parent;
                 x != (Edge*) 1;
                 x = verticies[x->from].parent) {
            x->addFlow(-1);
            ans.push_back(x->to);
        }
        ans.push_back(S);
        std::reverse(ans.begin(), ans.end());
        return ans;
    }
};


int main() {
    Graph g;
    g.read(std::cin);

    for(int i = 0; i < 2; i++) {
        g.makeLayeredNetwork();
        
       
//        std::cerr << "Distances: ";
//        for(auto v : g.verticies) {
//            std::cerr << v.distance << " ";
//        }
//        std::cerr << std::endl;
//
//        std::cerr << "Layered network edges:" << std::endl;
//        for(auto& vert : g.verticies)
//        for(auto v : vert.getLayeredIterable()) {
//            std::cerr << v->from << " " << v->to << " " << v->capacity << std::endl;
//        }

        if (g.verticies[g.T].distance == g.distInf) break;
        
        g.layeredNetDFS();
    }

    if (g.getFlow() != 2) {
        std::cout << "NO" << std::endl;
        return 0;
    }



    std::cout << "YES" << std::endl;
    auto ans1 = g.printPath();
    auto ans2 = g.printPath();
    for_each(ans1.begin(),ans1.end(),[](int x){std::cout << x + 1 << " ";});
    std::cout << std::endl;
    for_each(ans2.begin(),ans2.end(),[](int x){std::cout << x + 1 << " ";});
    std::cout << std::endl;
    return 0;
}
