#include "student.h"

void Student::readData()
{
    std::cout << "Enter bCode >> ";
    bCode = std::stoi(&readAndCheck(validateCode));
    std::cout << "Enter bName >> ";
    std::cin >> bName;
    std::cout << "Enter innings >> ";
    std::cin >> innings;
    std::cout << "Enter notout >> ";
    std::cin >> notout;
    std::cout << "Enter runs >> ";
    std::cin >> runs;

    batavg = calcAvg();
}

void Student::displayData()
{
    std::cout << "bCode:\t" << std::setfill('0')
              << std::setw(4) << bCode
              << std::endl;

    std::cout << "bName:\t"   << &bName[0] << std::endl;
    std::cout << "innings:\t" << innings << std::endl;
    std::cout << "notout:\t"  << notout << std::endl;
    std::cout << "runs:\t"    << runs << std::endl;
    std::cout << "batavg:\t"  << batavg << std::endl;
}

float Student::calcAvg()
{
    return (float) runs / (float) (innings - notout);
}
