#pragma once
#include "Metric.h"

class DistanceMetric : public Metric {
public:
    virtual std::string getReport(const Result& result, const OrderSet& orderSet) override;
    static double getPathLength(const Result& result, const OrderSet& orderSet, int path);
};