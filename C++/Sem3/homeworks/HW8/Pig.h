#pragma once
#include "Animal.h"
class Pig : public virtual Animal {
public:
    Pig(int id, Point position, double weight, double appetiteIndex);

    virtual double getDangerIndex() const;
    double getAppetiteIndex() const;
private:
    double appetiteIndex;
};


