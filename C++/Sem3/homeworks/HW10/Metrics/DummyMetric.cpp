#include "DummyMetric.h"

std::string DummyMetric::getReport(const Result &result, const OrderSet& orderSet)
{
    return "Well, at least the algorithm has finished ...\n";
}
