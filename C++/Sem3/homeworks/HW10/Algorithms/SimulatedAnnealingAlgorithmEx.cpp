#include "SimulatedAnnealingAlgorithmEx.h"
#include "DistanceMetric.h"
#include <stdexcept>
#include <math.h>
#include <limits>

bool SimulatedAnnealingAlgorithmEx::randomize()
{
    double len_old, len_new;
    if (1 || (rand() & 0xf) > 0xd) {
        size_t pathIdx = rand() % result.getPaths().size();
        auto& path = result.getPaths()[pathIdx];

        len_old = getPathWeight(pathIdx);
        std::swap(path[rand() % path.size()], path[rand() % path.size()]);
        len_new = getPathWeight(pathIdx);

        currentWeight = currentWeight - len_old + len_new;

        return true;
    }
    else {
        size_t pathIdxA = rand() % result.getPaths().size();
        size_t pathIdxB = rand() % result.getPaths().size();
        auto& pathA = result.getPaths()[pathIdxA];
        auto& pathB = result.getPaths()[pathIdxB];

        if (pathB.size() >= maxOrdersInPath || pathA.size() == 1) {
            return false;
        }

        len_old = getPathWeight(pathIdxA) + getPathWeight(pathIdxB);
        pathB.push_back(pathA.back());
        pathA.pop_back();
        len_new = getPathWeight(pathIdxA) + getPathWeight(pathIdxB);

        currentWeight = currentWeight - len_old + len_new;

        return true;
    }
}

double SimulatedAnnealingAlgorithmEx::getPathWeight(size_t path)
{
    return DistanceMetric::getPathLength(result, orderSet, path);
}

void SimulatedAnnealingAlgorithmEx::generateSomeSolution()
{
    result.getPaths().clear();
    result.getPaths().push_back({});

    int ordersNow = 0, maxOrdersInPath = orderSet.getParamI("maxOrdersInPath") - orderSet.getParamI("pathLengthMargin");

    if (maxOrdersInPath <= 0) {
        throw std::invalid_argument("Invalid argument for algorithm");
    }

    for (int i = orderSet.getOrders().size(); i--; ) {
        ++ordersNow;

        result.getPaths().back().push_back(i);

        if (ordersNow == maxOrdersInPath && i != 0) {
            ordersNow = 0;
            result.getPaths().push_back({});
        }
    }
}

Result SimulatedAnnealingAlgorithmEx::run(const OrderSet& orderSet)
{
    this->orderSet = orderSet;

    int iterations = orderSet.getParamI("iterations");
    int temperatureMagic = orderSet.getParamI("temperatureMagic");
    maxOrdersInPath = orderSet.getParamI("maxOrdersInPath");
    allowSwapPaths = orderSet.getParamI("allowSwapPaths");

    if (maxOrdersInPath < 0 || iterations < 0 || temperatureMagic < 0) {
        throw std::invalid_argument("Invalid argument for algorithm");
    }

    if (!haveInitialSolution) {
        generateSomeSolution();
    }
    haveInitialSolution = false;

    currentWeight = 0;
    for(size_t i = 0; i < result.getPaths().size(); i++) {
        currentWeight += getPathWeight(i);
    }

    // Optimize it
    double temperature;
    double oldDist = currentWeight;

    //std::cout << "Starting with dist = " << oldDist << std::endl;

    for (double i = 0; i < iterations; i++) {
        Result old = result;
        temperature = (iterations - i + 1) / temperatureMagic;

        randomize();

        double diff = (currentWeight - oldDist);
        if (diff <= 0) {
            oldDist = currentWeight;
            continue;
        }

        double prop = pow(2.7,(-diff / temperature));
        bool take = rand() <prop * std::numeric_limits <decltype(rand())>::max();
        if(!take) {
            result = old;
            currentWeight = oldDist;
        }

        oldDist = currentWeight;
    }

    //std::cout << "Optimized to dist = " << oldDist << std::endl;

    return result;
}

void SimulatedAnnealingAlgorithmEx::setInitialSolution(Result result)
{
    this->result = result;
    haveInitialSolution = true;
}


