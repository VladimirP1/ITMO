#include "Result.h"

Result::Result() {}

std::vector<std::vector<int> > &Result::getPaths()
{
    return paths;
}

const std::vector<std::vector<int> > &Result::getPaths() const
{
    return paths;
}

std::ostream& operator<<(std::ostream& out, const Result& task)
{
    for(auto& p : task.paths) {
        std::cout << "Path: ";
        for(auto j : p) {
            std::cout << j << " ";
        }
        std::cout << std::endl;
    }
    return out;
}
