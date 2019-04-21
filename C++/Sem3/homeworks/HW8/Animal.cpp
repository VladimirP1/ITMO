#include "Animal.h"

Animal::Animal(int id, Point position, double weight) : Unit(id, position), weight(weight)
{

}

double Animal::getWeight() const
{
    return weight;
}
