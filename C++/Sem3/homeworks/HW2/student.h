#pragma once
#include <iostream>
#include <iomanip>
#include <array>
#include <string.h>
#include <functional>
#include <string>

class Student
{
public:
    void readData();
    void displayData();
private:
    uint16_t bCode = 0;
    std::array <char, 21> bName{};
    int innings = 0;
    int notout = 0;
    int runs = 0;
    float batavg = 0;

    float calcAvg();

    template<typename T>
    T readAndCheck(const std::function<bool(T)>& func) {
        while(1) {
            T ret;
            std::cin >> ret;
            if(func(ret)) {
                return ret;
            }
            std::cout << "Invalid input. Try again >>";
        }
    }

    static bool validateCode(std::string str) {
        return str.size() == 4;
    }

};

template <long unsigned int N>
std::istream& operator>>(std::istream& in, std::array<char, N> & str) {
    while (1) {
        std::istream& result = in.getline(&str[0], N);
        if (strlen(&str[0]) > 0) {
            return result;
        }
    }
}
