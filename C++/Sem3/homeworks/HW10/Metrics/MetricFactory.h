#pragma once
#include "Metric.h"

class MetricFactory {
public:
    static Metric* createMetric(std::string name);
};
