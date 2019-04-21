#include "MetricFactory.h"
#include "StdDevMetric.h"
#include "DistanceMetric.h"
#include "DummyMetric.h"
#include "AverageDrivingDistMetric.h"
#include <stdexcept>

Metric *MetricFactory::createMetric(std::string name)
{
    if (name == "DistanceMetric") {
        return new DistanceMetric();
    }
    else if (name == "DummyMetric") {
        return new DummyMetric();
    }
    else if (name == "StdDevMetric") {
        return new StdDevMetric();
    }
    else if (name == "AverageDrivingDistMetric") {
        return new AverageDrivingDistMetric();
    }
    throw std::invalid_argument("Bad metric name");
}
