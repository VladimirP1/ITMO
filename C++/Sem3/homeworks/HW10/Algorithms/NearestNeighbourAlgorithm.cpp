#include "NearestNeighbourAlgorithm.h"
#include "DistanceMetric.h"
#include <stdexcept>
#include <limits>

Result NearestNeighbourAlgorithm::run(const OrderSet& orderSet)
{
    std::vector<bool> used(orderSet.getOrders().size());

    result.getPaths().clear();

    result.getPaths().push_back({});

    Order lastOrder;
    int ordersNow = 0, maxOrdersInPath = orderSet.getParamI("maxOrdersInPath");

    for (int i = orderSet.getOrders().size(); i--; ) {
        ++ordersNow;

        auto closer = [&](int a, int b) -> bool {
            auto distA = lastOrder.distance(orderSet.getOrders()[a]);
            auto distB = lastOrder.distance(orderSet.getOrders()[b]);
            if (used[a])
            {
                distA = std::numeric_limits<double>::infinity();
            }
            if (used[b])
            {
                distB = std::numeric_limits<double>::infinity();
            }
            return distA < distB;
        };

        int bestOrderIdx = 0;
        for (int j = 1; j < orderSet.getOrders().size(); j++) {
            if (!closer(bestOrderIdx, j)) {
                bestOrderIdx = j;
            }
        }

        used[bestOrderIdx] = true;
        result.getPaths().back().push_back(bestOrderIdx);
        lastOrder = orderSet.getOrders()[bestOrderIdx];

        if (ordersNow == maxOrdersInPath && i != 0) {
            ordersNow = 0;
            lastOrder = Order();
            result.getPaths().push_back({});
        }
    }

    return result;
}


