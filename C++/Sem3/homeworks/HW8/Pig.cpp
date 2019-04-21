#include "Pig.h"

Pig::Pig(int id, Point position, double weight, double appetiteIndex) : Unit(id, position), Animal(id, position, weight), appetiteIndex(appetiteIndex)
{

}

double Pig::getDangerIndex() const
{
    return getWeight() * appetiteIndex;
}

double Pig::getAppetiteIndex() const
{
    return appetiteIndex;
}
