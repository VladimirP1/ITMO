#pragma once
#include "Metric.h"

class DummyMetric : public Metric {
public:
    virtual std::string getReport(const Result& result, const OrderSet& orderSet) override;
};
