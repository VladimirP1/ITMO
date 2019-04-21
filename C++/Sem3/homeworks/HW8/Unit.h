#pragma once
#include "Point.h"

class Unit
{
public:
    Unit(int id, Point position);

    Point getPosition() const;
    void setPosition(Point point);
    int getId() const;
    void setId(int value);

    virtual ~Unit() = default;
private:
    int id = 0;
    Point position = Point(0,0);
};

