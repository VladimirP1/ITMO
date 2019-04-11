#include <iostream>
#include <fstream>
#include <deque>
#include <algorithm>
#include <vector>
#include <array>

//#define in std::cin
//#define out std::cout
std::ifstream in("fullham.in");
std::ofstream out("fullham.out");


class Graph {
public:
    Graph(std::istream& input) {
        input >> size;
        for(int i = 0; i < size; i++) {
            matrix[i][i] = 0;
            for(int j = 0; j < i; j++) {
                matrix[j][i] = matrix[i][j] = getNextZO();
            }
        }
    }

    bool getNextZO() {
        char c = 0;
        while ((c != '0') && (c != '1')) {
            in.get(c);
        }
        return c == '1';
    }

    bool hasEdge(int i, int j) {
        return matrix[i][j];
    }

    int getSize() const {
        return size;
    }
private:
    int size;
    std::array<std::array<bool, 4001>, 4001> matrix;
};
/*
class LinkedNode {
public:
    LinkedNode *next, *prev;
    LinkedNode(int value) : value(value) {}
    int getValue() { return value; }
private:
    int value;
};

class LinkedList {
public:
    LinkedList() {
        head = new LinkedNode(0);
        tail = new LinkedNode(0);
        head->prev = head;
        head->next = tail;
        tail->prev = head;
        tail->next = tail;
    }
    void pushTail(int value) {
        LinkedNode *node = new LinkedNode(value);
        node->prev = tail->prev;
        node->next = tail;
        tail->prev->next = node;
        tail->prev = node;
        size++;
    }

    LinkedNode* peekHead() {
        return head->next;
    }

    LinkedNode* popHead() {
        LinkedNode* ret = head->next;
        head->next = ret->next;
        ret->next->prev = head;
        size--;
        return ret;
    }

    bool empty() {
        return size <= 0;
    }
private:
    int size = 0;
    LinkedNode *head = new LinkedNode(0);
    LinkedNode *tail = new LinkedNode(0);
};
*/
Graph g(in);

int main() {
    int n =  g.getSize();
    std::deque<int> verts;

    for(int i = 0; i < n; i++) verts.push_back(i);

    for(int i = 0; i < n * (n - 1); i++) {
        if ( !g.hasEdge(verts.front(), *(verts.begin() + 1)) ) {
            auto j = verts.begin() + 2;
            while(!(g.hasEdge(*verts.begin(), *j) && g.hasEdge(*(verts.begin() + 1), *(j + 1)))){
                j++;
            }
            std::reverse(verts.begin() + 1, j + 1);
        }
        verts.push_back(verts.front());
        verts.pop_front();
    }
    for(int i = 0; i < n ; i++) {
        out << (verts.front() + 1) << " ";
        verts.pop_front();
    }
}
