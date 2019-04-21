cmake_minimum_required(VERSION 2.8)

project(HW10)
include_directories("Metrics" "Utility" "Algorithms")
add_executable(${PROJECT_NAME} "main.cpp"
    "Algorithms/Algorithm.cpp"
    "Algorithms/DummyAlgorithm.cpp"
    #"Algorithms/SimulatedAnnealingAlgorithm.cpp"
    "Algorithms/SimulatedAnnealingAlgorithmEx.cpp"
    "Algorithms/NearestNeighbourAlgorithm.cpp"
    "Algorithms/NearestNeighbourAnnealedAlgorithm.cpp"
    "Algorithms/AlgorithmFactory.cpp"
    "Utility/Order.cpp"
    "Utility/OrderSet.cpp"
    "Utility/Result.cpp"
    "Metrics/Metric.cpp"
    "Metrics/DummyMetric.cpp"
    "Metrics/DistanceMetric.cpp"
    "Metrics/StdDevMetric.cpp"
    "Metrics/AverageDrivingDistMetric.cpp"
    "Metrics/MetricFactory.cpp"
)
