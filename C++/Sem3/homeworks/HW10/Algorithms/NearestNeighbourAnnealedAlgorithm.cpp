#include "NearestNeighbourAnnealedAlgorithm.h"
#include "NearestNeighbourAlgorithm.h"
#include "SimulatedAnnealingAlgorithmEx.h"
#include "DistanceMetric.h"

Result NearestNeighbourAnnealedAlgorithm::run(const OrderSet& orderSet)
{
    auto initialSolution = NearestNeighbourAlgorithm().run(orderSet);
    auto finalisingAlgorithm = SimulatedAnnealingAlgorithmEx();
    finalisingAlgorithm.setInitialSolution(initialSolution);
    return finalisingAlgorithm.run(orderSet);
}


