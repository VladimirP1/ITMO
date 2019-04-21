#include "AlgorithmFactory.h"
#include "NearestNeighbourAlgorithm.h"
#include "SimulatedAnnealingAlgorithmEx.h"
#include "NearestNeighbourAnnealedAlgorithm.h"
#include "DummyAlgorithm.h"

#include <stdexcept>

Algorithm *AlgorithmFactory::createAlgorithm(std::string name)
{
    if(name == "NearestNeighbourAlgorithm") {
        return new NearestNeighbourAlgorithm();
    }
    else if(name == "SimulatedAnnealingAlgorithmEx") {
        return new SimulatedAnnealingAlgorithmEx();
    }
    else if(name == "DummyAlgorithm") {
        return new DummyAlgorithm();
    }
    else if(name == "NearestNeighbourAnnealedAlgorithm") {
        return new NearestNeighbourAnnealedAlgorithm();
    }
    throw std::invalid_argument("Bad algo name");
}
