#pragma once
#include <string>
#include "OrderSet.h"
#include "Result.h"

class Algorithm {
public:
    virtual Result run(const OrderSet& orderSet) = 0;
    virtual ~Algorithm();
protected:
    Algorithm() {};
};
