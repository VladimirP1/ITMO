#pragma once
#include "Algorithm.h"

class NearestNeighbourAlgorithm: public Algorithm {
public:
    Result run(const OrderSet &orderSet) override;
private:
    Result result;
    int maxOrdersInPath;
};
