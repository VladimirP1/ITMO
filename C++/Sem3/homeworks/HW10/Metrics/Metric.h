#pragma once
#include <string>
#include "Result.h"
#include "OrderSet.h"

class Metric {
public:
    virtual std::string getReport(const Result& result, const OrderSet& orderSet) = 0;
    virtual ~Metric();
protected:

};
