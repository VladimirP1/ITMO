#pragma once
#include <map>
#include <vector>
#include "Order.h"

class OrderSet {
public:
    void addOrder(const Order& order);
    const std::vector<Order>& getOrders() const;

    void readFromFile(std::string filename);

    int getParamI(std::string name) const;
private:
    std::map<std::string, int> params;
    std::vector<Order> orders;
};
