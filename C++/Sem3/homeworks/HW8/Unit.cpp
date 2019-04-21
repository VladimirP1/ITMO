#include "Unit.h"

Unit::Unit(int id, Point position) : id(id), position(position)
{

}

Point Unit::getPosition() const
{
    return position;
}

void Unit::setPosition(Point point)
{
    position = point;
}

int Unit::getId() const
{
    return id;
}

void Unit::setId(int value)
{
    id = value;
}
