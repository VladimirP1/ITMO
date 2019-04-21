#include <iostream>
#include <vector>
#include <string>
#include "ManBearPig.h"
#include "TasksExcept1.h"
#include "TestClasses.h"

int a = 0;

int paramFunction(double d) {
    return d;
}

int* returnedFunction(char const* c) {
    return &a;
}

ReturnedFunction complexFunction(int i, ParamFunction p) {
    (*p)(1.0);
    return &returnedFunction;
}

int main()
{


    // -------
    std::string s1("Elf");
    std::string s2("Archer");

    bool r1 = compare(s1, s2, &std::string::size);
    bool r2 = compare(s1, s1, &std::string::size);

    std::cout << r1 << " " << r2 << std::endl;

    // --------

    Top top;
    L* l = dynamic_cast<L*>(&top);
    R* r = dynamic_cast<R*>(&top);
    Base* base_l = dynamic_cast<Base*>(l);
    Base* base_r = dynamic_cast<Base*>(r);

    std::cout << isSameObject(base_l, base_r) <<
                 isSameObject(base_l, base_l) <<
                 (base_l == base_r) << std::endl;

    // --------

    std::vector<Unit*> units;
    for(int i = 0; i < 100; i++) {
        if(rand() & 1) {
            units.push_back(new ManBearPig(i, Point(rand() % 100, rand() % 100), rand() % 300, rand() % 130, rand() % 10, rand() % 100));
        } else {
            units.push_back(new Pig(i, Point(rand() % 100, rand() % 100), rand() % 200, rand() % 100));
        }
    }
    units.push_back(new Man(100, Point(-2, -2), 200));
    while(true) {
        double closest = 1000, dist;
        int closestIdx = -1;
        for(int i = 0; i < 100; i++) {
            if(dynamic_cast<Man*>(units[100])->isAnimalLethal(*dynamic_cast<Animal*>(units[i]))) {
               std::cout << "You have died" << std::endl;
               return 1;
            }
            if((dist = units[i]->getPosition().distance(units[100]->getPosition())) < closest){
                closest = dist;
                closestIdx = i;
            }
        }

        auto newPosition = units[100]->getPosition();
        std::cout << "Closest entity is at distance " << closest << " your pos: " << newPosition.getX() << " " << newPosition.getY() << std::endl;
        char cmd;
        std::cin >> cmd;
        switch(cmd) {
        case 'w':
            newPosition += Point(0, 1);
            break;
        case 'a':
            newPosition +=  Point(-1, 0);
            break;
        case 's':
            newPosition += Point(0, -1);
            break;
        case 'd':
            newPosition += Point(1, 0);
            break;
        }
        units[100]->setPosition(newPosition);
        if(newPosition == Point(100,100)) {
            std::cout << "You have won" << std::endl;
            return 0;
        }

    }



    return 0;
}
