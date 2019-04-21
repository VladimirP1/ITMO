#include "StdDevMetric.h"
#include "DistanceMetric.h"
#include <math.h>
#include <numeric>

void StdDevMetric::calculateDistances(const Result &result, const OrderSet &orderSet)
{
    distances.resize(result.getPaths().size());
    for(int idx = 0; idx < result.getPaths().size(); ++idx) {
        distances[idx] = DistanceMetric::getPathLength(result, orderSet, idx);
    }
}

std::string StdDevMetric::getReport(const Result &result, const OrderSet &orderSet)
{
    calculateDistances(result, orderSet);
    double stdDev = calculateStdDev();
    return "Standard deviation: " + std::to_string(stdDev);
}

double StdDevMetric::calculateStdDev()
{
    double E = std::accumulate(distances.begin(), distances.end(), 0) / distances.size();
    double stdDev = 0;
    for (auto & x : distances) {
        stdDev += (x - E) * (x - E);
    }
    stdDev = sqrt(stdDev);

    return stdDev;
}



