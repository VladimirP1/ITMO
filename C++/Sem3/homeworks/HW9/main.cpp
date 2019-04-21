#include <iostream>
#include <sstream>

#include "templates_impl.hpp"
#include "array.h"

void test_array(){
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
}

int main()
{
    print_values(1, "3");
    auto t = std::make_tuple(0, 3.5, "Hello");
    std::pair<double, char const *> p = to_pair<1,2>(t);
    print_values(p.first, p.second);
    test_array();
    return 0;
}
