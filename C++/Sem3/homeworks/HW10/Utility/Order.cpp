#include "Order.h"
#include <cmath>

Order::Order()
{
}

Order::Order(double x, double y) :
    coordinateX(x), coordinateY(y)
{
}

std::pair<double, double> Order::getCoordinates() const
{
    return std::make_pair(coordinateX, coordinateY);
}

double Order::distance(const Order &other) const
{
    return sqrt(
               pow(coordinateX - other.coordinateX, 2) +
               pow(coordinateY - other.coordinateY, 2)
           );
}
