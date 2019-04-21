#pragma once
#include "Animal.h"

class Bear : public virtual Animal {
public:
    Bear(int id, Point position, double weight, double damageIndex);
    virtual double getDangerIndex() const;
    double getDamageIndex() const;

private:
    double damageIndex;
};

