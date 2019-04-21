#include "Man.h"
#include "Animal.h"

Man::Man(int id, Point position, double iq) : Unit (id, position), iq(iq)
{

}

bool Man::isAnimalLethal(const Animal &animal) const
{
    if (animal.getPosition().distance(getPosition()) >= 1.0) {
        return false;
    }
    return animal.getDangerIndex() > 10 * iq;
}

double Man::getIq() const
{
    return iq;
}


