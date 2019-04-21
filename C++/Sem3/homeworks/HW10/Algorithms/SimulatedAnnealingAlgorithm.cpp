#include "SimulatedAnnealingAlgorithm.h"
#include <stdexcept>
#include <algorithm>
#include <math.h>
#include <limits>

SimulatedAnnealingAlgorithm::SimulatedAnnealingAlgorithm(const OrderSet &orderSet, std::string metric) : Algorithm (orderSet, metric)
{

}

void SimulatedAnnealingAlgorithm::randomize(Result& task)
{
    size_t pathIdx = rand() % task.getPathsCount();
    auto& path = task.getPath(pathIdx);
    std::swap(path[rand() % path.size()], path[rand() % path.size()]);
    metric->notifyPathChanged(pathIdx);
}

Result SimulatedAnnealingAlgorithm::run()
{
    Result result;
    metric->bindTask(&result, &orderSet);
    metric->reset();

    // Build some solution

    result.addPath();

    int ordersNow = 0, maxOrdersInPath = orderSet.getParamI("maxOrdersInPath");
    int iterations = orderSet.getParamI("iterations");
    int temperatureMagic = orderSet.getParamI("temperatureMagic");

    if (maxOrdersInPath < 0 || iterations < 0 || temperatureMagic < 0) {
        throw std::invalid_argument("Invalid argument for algorithm");
    }

    for (int i = orderSet.getOrders().size(); i--; ) {
        ++ordersNow;

        result.getPath(result.getPathsCount() - 1).push_back(i);

        if (ordersNow == maxOrdersInPath && i != 0) {
            ordersNow = 0;
            result.addPath();
        }
    }

    for (int i = 0; i < result.getPathsCount(); ++i) {
        metric->notifyPathChanged(i);
    }

    // Optimize it

    double temperature;
    double oldDist = metric->recalculate();

    std::cout << "Starting with dist = " << oldDist << std::endl;

    for (double i = 0; i < iterations; i++) {
        Result old = result;
        temperature = (iterations - i + 1) / temperatureMagic;

        randomize(result);

        double diff = (metric->recalculate() - oldDist);
        if (diff <= 0) {
            continue;
        }

        double prop = pow(2.7,(-diff/temperature));
        bool take = rand() < prop * std::numeric_limits < decltype(rand())>::max();
        if(!take) {
            result = old;
        }

        oldDist = metric->recalculate();
    }

    std::cout << "Optimized to dist = " << oldDist << std::endl;

    return result;
}
