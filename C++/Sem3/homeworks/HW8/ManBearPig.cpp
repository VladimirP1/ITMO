#include "ManBearPig.h"

ManBearPig::ManBearPig(int id, Point position, double weight, double iq, double damageIndex, double appetiteIndex) :
    Unit (id, position), Animal(id, position, weight), Man(id, position, iq), Bear(id, position, weight, damageIndex), Pig(id, position, weight, appetiteIndex)
{

}

double ManBearPig::getDangerIndex() const
{
    return Pig::getDangerIndex() + Bear::getDangerIndex();
}
