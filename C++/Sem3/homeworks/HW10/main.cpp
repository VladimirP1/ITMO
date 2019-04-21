#include <iostream>
#include <fstream>
#include "AlgorithmFactory.h"
#include "MetricFactory.h"

int main()
{
    srand(time(nullptr));

    std::ifstream in("config.txt");
    std::string algoName, metricName;
    in >> algoName >> metricName;

    OrderSet s;
    s.readFromFile("orders.txt");

    std::cout << algoName << " " << metricName << std::endl;

    Algorithm* algo = AlgorithmFactory::createAlgorithm(algoName);
    Metric* metric = MetricFactory::createMetric(metricName);

    Result result = algo->run(s);

    std::string report = metric->getReport(result, s);

    std::cout << result << std::endl;
    std::cout << report << std::endl;

    delete algo;
    delete metric;

    return 0;
}
