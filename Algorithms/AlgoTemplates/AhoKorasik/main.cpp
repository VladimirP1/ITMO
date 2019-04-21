#include <algorithm>
#include <iostream>
#include <memory>

std::string transformString(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), [](char c){ return c - 'a'; });   
    return str;
}

std::string untransformString(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), [](char c){ return c + 'a'; });   
    return str;
}

template<size_t A>
class Node {
public:
    Node() {
        std::fill_n(_children, A, (Node*)0);
    }
    ~Node() {
        std::for_each(_children, _children + 26, [](Node* p){ if(p) delete p; });
    }
    Node* addChild(size_t c) {
        if (!_children[c]) {
            _children[c] = new Node();
            _children[c]->_parent = this;
            _children[c]->_parentChar = c;
        }
        return _children[c];
    }
    
    Node* getChild(size_t c) {
        return _children[c];
    }
    
    bool haveChild(size_t c) {
        return _children[c];
    }
    
    void setTerminal(bool isTerminal) { _isTerminal = isTerminal;  }
    bool isTerminal () const          { return _isTerminal;        }
    bool isRoot()      const          { return _parent == this;    }
    
    
    Node* suffixLink() {
        if (suffixLinkCached) {
            return suffixLinkCached;
        }
        if (isRoot() || _parent->isRoot()) {
            return suffixLinkCached = _parent;
        }
        return suffixLinkCached = _parent->suffixLink()->proceed(_parentChar);
    }
    
    Node* proceed(size_t c) {
        if (haveChild(c)) {
            return getChild(c);
        }
        if (isRoot()) {
            return this;
        }
        return _children[c] = suffixLink()->proceed(c);
        
    }
    
    Node* compressedSuffixLink() {
        if (compressedSuffixLinkCached) {
            return compressedSuffixLinkCached;
        }
        if (isRoot() || _parent->isRoot()) {
            return compressedSuffixLinkCached = _parent;
        }
        if (suffixLink()->isTerminal()) {
            return compressedSuffixLinkCached = suffixLink();
        }
        return compressedSuffixLinkCached = suffixLink()->compressedSuffixLink();
        
    }
    std::string debugString = transformString("root");
private:
    bool _isTerminal;
    Node* _children[A];
    Node* _parent = this;
    size_t _parentChar = 255;

    Node* suffixLinkCached = nullptr;
    Node* compressedSuffixLinkCached = nullptr;
    
};

using LatinNode = Node<26>;


class Trie {
public:
    Trie() {
        root.reset(new LatinNode());
    }
    
    void addString(std::string str) {        
        LatinNode* cur = root.get();
        int x = 0;
        for (auto c : str) {
            cur = cur->addChild(c);
            cur->debugString = str.substr(0, ++x);
        }
        cur->setTerminal(true);
    }
    
    void matchString(std::string str) {
        auto x = root.get();
        
        for (auto c : str) {
            x = x->proceed(c);
            if (x->isTerminal()) {
                auto z = x;
                
                while(z->isTerminal() && !z->isRoot()) {
                    std::cout << "Match: " << untransformString(z->debugString) << std::endl;
                    z = z->compressedSuffixLink();
                }
            }
        }
    }
    
    LatinNode* getRoot() {
        return root.get();
    }
private:
    std::unique_ptr<LatinNode> root;
    
};

void printTrie(LatinNode* start, int indent = 0) {
    for(int i = 0; i < indent; i++) std::cout << "|";
    std::cout << untransformString(start->debugString) << std::endl;
    for(int i = 0; i < 26; i++) {
        if(start->haveChild(i)) {
            printTrie(start->getChild(i), indent + 1);
        }
    }
    
}

int main(int argc, char **argv) {
    Trie t;
    t.addString(transformString("aba"));
    t.addString(transformString("ada"));
    t.addString(transformString("abacabapaba"));
    t.addString(transformString("bsdadaba"));
    t.addString(transformString("abarcadabas"));
    
    printTrie(t.getRoot());
    
    t.matchString(transformString("dabadaabsdadaba"));
    
    
    return 0;
}
