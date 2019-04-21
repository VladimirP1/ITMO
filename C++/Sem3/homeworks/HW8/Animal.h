#pragma once
#include "Unit.h"

class Animal : public virtual Unit {
public:
    Animal(int id, Point position, double weight);
    virtual double getDangerIndex() const = 0;
    double getWeight() const;

private:
    double weight;
};

