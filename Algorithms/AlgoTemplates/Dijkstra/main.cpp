#include <algorithm>
#include <iostream>
#include <memory>
#include <vector>
#include <queue>

class Edge {
public:
    Edge(int weight) : _weight(weight) {}
    
    int weight() { return _weight; }
private:
    int _weight;
};

class Vertex {
public:
    template<class T>
    void forEachConnectedVertex(T&& lambda) {
        for(size_t i = 0; i < _edges.size(); i++) {
            lambda(_edge_props[i], _edges[i]);
        }
    }
    
    size_t id() {
        return _id;
    }
    
    //Dijkstra-specific
    int distanceEst = std::numeric_limits<int>::max();
    bool used = false;
    
private:
    friend class Graph;
    size_t _id;
    std::vector<Vertex*> _edges; 
    std::vector<Edge*> _edge_props;
    
};

class Graph {
public:
    Graph(size_t n) {
        _verticies.resize(n);
        size_t i = 0;
        for_each(_verticies.begin(),_verticies.end(), [n, &i](std::unique_ptr<Vertex>& ptr){
            ptr.reset(new Vertex);
            ptr.get()->_id = i++;
        });
    }
    
    void addEdge(size_t start, size_t end, Edge const& e) {
        _edges.push_back({});
        _edges.back().reset(new Edge(e));
        _verticies[start]->_edges.push_back(_verticies[end  ].get());
        _verticies[end  ]->_edges.push_back(_verticies[start].get());
        _verticies[start]->_edge_props.push_back(_edges.back().get());
        _verticies[end  ]->_edge_props.push_back(_edges.back().get());
    }
    
    Vertex* getVertex(size_t i) {
        return _verticies[i].get();
    }
    
    template<class T>
    void forEachVertex(T&& lambda) {
        std::for_each(_verticies.begin(), _verticies.end(), [&lambda](const std::unique_ptr<Vertex>& ptr){ lambda(ptr.get()); });
    }
private:
    std::vector<std::unique_ptr<Vertex>> _verticies;
    std::vector<std::unique_ptr<Edge>> _edges;
    
};

void dijkstra() {
    
}

int main(int argc, char **argv) {
    Graph g(10);
    g.addEdge(0, 1, Edge(1));
    g.addEdge(1, 2, Edge(4));
    g.addEdge(2, 0, Edge(1));
        
    using QueueValueType = std::pair<int, Vertex*>;
    std::priority_queue
        <
            QueueValueType,
            std::vector<QueueValueType>,
            std::greater<QueueValueType> 
        > Q;
    
    g.getVertex(0)->distanceEst = 0;
    Q.push({0, g.getVertex(0)});
    
    while (!Q.empty()) {
        auto currentVertex = Q.top();
        Q.pop();
        
        if(currentVertex.second->used) {
            continue;
        }
        
        currentVertex.second->forEachConnectedVertex([&from = currentVertex.second, &Q](Edge* edge, Vertex* to){
            if(!to->used) {
                to->distanceEst = std::min(to->distanceEst, from->distanceEst + edge->weight());
                Q.push({to->distanceEst, to});
            }
        });
        
        currentVertex.second->used = true;
    }
    
    g.forEachVertex([](Vertex* v){ std::cout << v->distanceEst << std::endl; });
    
    
    
    return 0;
}
