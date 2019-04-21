#include "OrderSet.h"
#include <stdexcept>
#include <fstream>
#include <sstream>

void OrderSet::addOrder(const Order &order)
{
    orders.push_back(order);
}

void OrderSet::readFromFile(std::string filename)
{
    int n;
    std::string line;
    std::ifstream in(filename);
    if (!in.good()) {
        throw std::invalid_argument("Bad file name");
    }
    {
        in >> n;
        for (int i = 0; i < n; ++i) {
            std::getline(in, line);
            std::stringstream ss(line);
            double x, y;
            ss >> x >> y;
            orders.push_back({x,y});
        }
    }
    {
        in >> n;
        int value;
        std::string param;
        for(int i = 0; i < n; ++i) {
            in >> param >> value;
            params[param] = value;
        }
    }
    in.close();
}

int OrderSet::getParamI(std::string name) const
{
    auto i = params.find(name);
    if (i != params.end()) {
        return i->second;
    }
    return -1;
}

const std::vector<Order>& OrderSet::getOrders() const
{
    return orders;
}
