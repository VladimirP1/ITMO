#pragma once
#include <iostream>
#include <tuple>

void print_values() {

}

template <class T, class... Args>
void print_values(T value, Args... args) {
    std::cout << typeid(value).name() << ": " << value << std::endl;
    print_values(args...);
}

template <int i1, int i2, class... Types>
auto to_pair(std::tuple <Types...> t) ->
    decltype (std::make_pair(std::get<i1>(t), std::get<i2>(t))) {
    return std::make_pair(std::get<i1>(t), std::get<i2>(t));
}
