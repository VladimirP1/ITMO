#include "AverageDrivingDistMetric.h"
#include "DistanceMetric.h"

std::string AverageDrivingDistMetric::getReport(const Result& result, const OrderSet& orderSet)
{
    int drivingCount = 0;
    double distance = 0;
    for(int  i = 0; i < result.getPaths().size(); ++i) {
        distance += DistanceMetric::getPathLength(result, orderSet, i);
        drivingCount += result.getPaths()[i].size();
    }
    return "Average driving distance: " + std::to_string(distance / drivingCount);
}
