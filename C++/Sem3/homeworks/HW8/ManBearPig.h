#pragma once
#include "Man.h"
#include "Bear.h"
#include "Pig.h"

class ManBearPig : public Man, public Bear, public Pig {
public:
    ManBearPig(int id, Point position, double weight, double iq, double damageIndex, double appetiteIndex);
    virtual double getDangerIndex() const;
};

