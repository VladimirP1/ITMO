#pragma once
#include <string>

class Order {
public:
    Order();
    Order(double x, double y);
    std::pair<double, double> getCoordinates() const;
    double distance(const Order& other) const;
private:
    double coordinateX = 0;
    double coordinateY = 0;
};
