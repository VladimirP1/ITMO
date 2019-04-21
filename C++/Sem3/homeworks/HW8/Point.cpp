#include "Point.h"
#include <cmath>

Point::Point(int x, int y) : x(x), y(y)
{

}

double Point::distance(const Point &point) const
{
    return sqrt(pow(x - point.x, 2) + pow(y - point.y, 2));
}

Point Point::operator+(const Point &point) const
{
    Point ret = point;
    return ret += point;
}

Point &Point::operator+=(const Point &point)
{
    x += point.x;
    y += point.y;
    return *this;
}

bool Point::operator==(const Point &point) const
{
    return x == point.x && y == point.y;
}

int Point::getX() const
{
    return x;
}

int Point::getY() const
{
    return y;
}
