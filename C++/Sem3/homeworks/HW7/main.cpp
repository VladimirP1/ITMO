#include <iostream>
#include <sstream>
#include "array.h"

class noDefCtor{
public:
    noDefCtor(int) {}
    noDefCtor(const noDefCtor&) {}
    noDefCtor& operator=(const noDefCtor&) = delete;
};


bool less(int a, int b) {
    return a < b;
}

struct Greater {
    template<typename T>
    bool operator()(T a, T b) const {
        return b < a;
    }
};

int main()
{
    using T = int;

    Array<T>* test[10];
    std::stringstream stream;
    for(int i = 0; i < 10; i++) {
        test[i] = new Array<int>();
    }

    for(int i = 0; i < 1000; i++) {
        int a = rand() % 10;
        int b = rand() % 10;
        while(a == b) b = rand() % 10;
        int action = rand() % 9;
        switch(action) {
        case 0:
            delete test[a];
            test[a] = new Array<T>();
            break;
        case 1:
            delete test[a];
            test[a] = new Array<T>(rand()%1000, rand());
            break;
        case 2:
            delete test[a];
            test[a] = new Array<T>(*test[b]);
            break;
        case 3:
            delete test[a];
            test[a] = new Array<T>(std::move(*test[b]));
            break;
        case 4:
            *test[a] = *test[b];
            break;
        case 5:
            *test[a] = std::move(*test[b]);
            break;
        case 6:
            flatten(*test[a], stream);
            break;
        case 7:
            if(test[a]->size()) {
                minimum(*test[a], less);
            }
            break;
        case 8:
            if(test[a]->size()) {
                (*test[a])[static_cast<unsigned int>(rand()) % test[a]->size()] = rand();
            }
            break;
        }

    }

    for(int i = 0; i < 10; i++) {
        delete test[i];
    }

    Array<noDefCtor> a(1, noDefCtor(1));
    Array<noDefCtor> b(1, noDefCtor(1));
    a = b;
    b = a;
    a = std::move(b);

    Array<int> A(10, 1);
    A[5] = 4;
    A[3] = -1;
    flatten(A, std::cout);
    std::cout << std::endl;
    std::cout << minimum(A, less) << std::endl;
    std::cout << minimum(A, Greater()) << std::endl;



    return 0;
}
