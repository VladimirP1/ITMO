#include "DistanceMetric.h"

std::string DistanceMetric::getReport(const Result& result, const OrderSet& orderSet)
{
    double distance = 0;
    for(int  i = 0; i < result.getPaths().size(); ++i) {
        distance += getPathLength(result, orderSet, i);
    }
    return "Total paid distance: " + std::to_string(distance);
}

double DistanceMetric::getPathLength(const Result &result, const OrderSet &orderSet, int path)
{
    double distance = 0;

    Order prev;
    for(const auto& pointIdx : result.getPaths()[path]) {
        const auto& order = orderSet.getOrders()[pointIdx];

        distance += prev.distance(order);

        prev = order;
    }

    return distance;
}


