#pragma once
#include "Algorithm.h"

class AlgorithmFactory {
public:
    static Algorithm* createAlgorithm(std::string name);
};
