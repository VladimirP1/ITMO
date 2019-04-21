#include "Bear.h"

Bear::Bear(int id, Point position, double weight, double dangerIndex) : Unit(id,position), Animal (id, position, weight), damageIndex(dangerIndex) {

}

double Bear::getDangerIndex() const
{
    return damageIndex;
}

double Bear::getDamageIndex() const
{
    return damageIndex;
}
