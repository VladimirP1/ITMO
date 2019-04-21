#include "BadFromString.h"
#include <ios>
#include <iostream>
#include <sstream>
#include <functional>

template<class T>
T from_string(const std::string& input) {
    T result;
    std::istringstream inputStream(input);
    inputStream >> std::noskipws >> result;

    if (inputStream.fail()) {
        throw bad_from_string("Bad input: operator>> failed");
    }
    if (inputStream.peek() != EOF) {
        throw bad_from_string("Bad input: trash after token");
    }

    return result;
}

template<typename T>
void testException(std::string in) {

    try {
        from_string<T>(in);
    } catch (bad_from_string&) {
        std::cout << "Error converting \"" << in << "\" to " << typeid(T).name() << std::endl;
        return;
    }
    std::cout << "Converted \"" << in << "\" to " << typeid(T).name() << std::endl;
}

int main() {
    const std::string tests[] = {"123", "12.3", "abc", "abc ", " abc", "x", "x ", " x", "x.", "0."};
    for (auto test : tests) {
        testException<int>(test);
        testException<double>(test);
        testException<std::string>(test);
        testException<char>(test);
    }

    return 0;
}


