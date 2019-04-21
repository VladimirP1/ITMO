#pragma once
#include "Unit.h"
#include "Animal.h"

class Man : public virtual Unit {
public:
    Man(int id, Point position, double iq);

    bool isAnimalLethal(const Animal& animal) const;
    double getIq() const;
private:
    double iq;
};

