#include "DummyAlgorithm.h"

Result DummyAlgorithm::run(const OrderSet& orderSet)
{
    Result result;

    result.getPaths().push_back({});

    int ordersNow = 0, maxOrdersInPath = orderSet.getParamI("maxOrdersInPath");
    for (int i = orderSet.getOrders().size(); i--; ) {
        ++ordersNow;

        result.getPaths().back().push_back(i);

        if (ordersNow == maxOrdersInPath && i != 0) {
            ordersNow = 0;
            result.getPaths().push_back({});
        }
    }

    return result;
}
