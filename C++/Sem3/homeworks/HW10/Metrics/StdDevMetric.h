#pragma once
#include "Metric.h"

class StdDevMetric : public Metric {
public:
    virtual std::string getReport(const Result& result, const OrderSet& orderSet) override;
private:
    std::vector<double> distances;
    double calculateStdDev();
    void calculateDistances(const Result& result, const OrderSet& orderSet);
};
