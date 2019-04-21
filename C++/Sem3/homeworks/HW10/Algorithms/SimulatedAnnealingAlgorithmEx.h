#pragma once
#include "Algorithm.h"

class SimulatedAnnealingAlgorithmEx: public Algorithm {
public:
    Result run(const OrderSet &orderSet) override;
    void setInitialSolution(Result result);
private:
    OrderSet orderSet;
    Result result;
    bool allowSwapPaths;
    bool haveInitialSolution = false;
    double currentWeight;
    int maxOrdersInPath;

    bool randomize();
    double getPathWeight(size_t path);
    void generateSomeSolution();
};
