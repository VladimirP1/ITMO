#include <iostream>
#include <vector>


struct SegTreeVertex {
    int upd = -1;
    int x = 0;
    int calc() {
        return std::max(x, upd);
    }
};

class SegTree {
public:
    SegTree(size_t n);

    void update(size_t l, size_t r, int set);
    int query(size_t l, size_t r);
    int query(size_t point);
private:
    size_t sizeRequested;
    size_t sizeActual;
    std::vector<SegTreeVertex> data;

    size_t getBestPowerOfTwo(size_t x);
    int update(size_t i, size_t l, size_t r, size_t curL, size_t curR, int set);
    int query(size_t i, size_t l, size_t r, size_t curL, size_t curR);
    void push(size_t i);
};

SegTree::SegTree(size_t n) {
    sizeActual = getBestPowerOfTwo(sizeRequested = n);
    data.resize(2 * sizeActual);
}

void SegTree::update(size_t l, size_t r, int set)
{
    update(0, l, r, 0, sizeActual, set);
}

int SegTree::query(size_t l, size_t r)
{
    return query(0, l, r, 0, sizeActual);
}

int SegTree::query(size_t point)
{
    return query(point, point + 1);
}

int SegTree::update(size_t i, size_t l, size_t r, size_t curL, size_t curR, int set)
{
    if (curL >= r || curR <= l) { // No intersection with requested range
        return 0;
    } else if(l <= curL && curR <= r) { // Inside requested range
        data[i].upd = set;
        return set;
    } else {
        push(i);
        int ret = std::max(
                    update(2*i + 1, l, r, curL, curL + (curR - curL) / 2, set),
                    update(2*i + 2, l, r, curL + (curR - curL) / 2, curR, set)
                    );
        data[i].x = ret;
        return ret;
    }
}

int SegTree::query(size_t i, size_t l, size_t r, size_t curL, size_t curR)
{
    if (curL >= r || curR <= l) { // No intersection with requested range
        return 0;
    } else if(l <= curL && curR <= r) { // Inside requested range
        return data[i].calc();
    } else {
        push(i);
        int ret = std::max(
                    query(2*i + 1, l, r, curL, curL + (curR - curL) / 2),
                    query(2*i + 2, l, r, curL + (curR - curL) / 2, curR)
                    );
        return ret;
    }
}

void SegTree::push(size_t i)
{
    if(data[i].upd >= 0) {
        data[2*i + 1].upd = data[2*i + 2].upd = data[i].upd;
        data[i].upd = -1;
        data[i].x = data[2*i + 1].upd;
    }
}

size_t SegTree::getBestPowerOfTwo(size_t x) {
    size_t i = 1;
    while(i < x) i*=2;
    return i;
}

SegTree* t[2] = {nullptr, nullptr};

void initTrees() {
    if(!t[0]) t[0] = new SegTree(120);
    if(!t[1]) t[1] = new SegTree(120);
}

bool addEdge(int tree, size_t a, size_t b) {
    if (a > b) std::swap(a, b);
    if ((t[tree]->query(a) == 0) && (t[tree]->query(b) == 0)) {
        t[tree]->update(a + 1, b, 1);
        return true;
    }
    return false;
}
bool addEdgeV(int tree, size_t a, size_t b) {
    bool result = addEdge(tree, a, b);
    std::cout << "Adding edge " << a << " -> " << b << "  to tree " << tree << "  result: " << result << std::endl;
    return result;
}

int main()
{
    initTrees();
    addEdgeV(0, 0, 1);
    addEdgeV(0, 0, 2);
    addEdgeV(0, 3, 1);
    addEdgeV(1, 3, 1);



    return 0;
}
