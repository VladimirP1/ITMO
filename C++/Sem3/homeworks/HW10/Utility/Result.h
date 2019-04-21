#pragma once
#include <vector>
#include <iostream>

class Result {
public:
    Result();
    std::vector<std::vector<int>>& getPaths();
    const std::vector<std::vector<int>>& getPaths() const;

    friend std::ostream& operator<<(std::ostream& out, const Result& task);
private:
    std::vector<std::vector<int>> paths;
};

std::ostream& operator<<(std::ostream& out, const Result& task);
