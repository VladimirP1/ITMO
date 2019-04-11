#include <iostream>
#include <vector>
#include <cmath>
#include <set>

void intersect(double x_a_0, double y_a_0, double x_a_1, double y_a_1,
               double x_b_0, double y_b_0, double x_b_1, double y_b_1,
               double& int_x, double& int_y, double& detA, double& k0) {
    double t_a_x = x_a_1 - x_a_0;
    double t_a_y = y_a_1 - y_a_0;
    double t_b_x = x_b_1 - x_b_0;
    double t_b_y = y_b_1 - y_b_0;

    double dp0_x = x_b_0 - x_a_0;
    double dp0_y = y_b_0 - y_a_0;

    detA = t_a_x * t_b_y - t_a_y * t_b_x;

    k0 = (t_b_y * dp0_x - t_b_x * dp0_y)/detA;
    //double k1 = (- t_a_y * dp0_x + t_a_x * dp0_y)/detA;

    int_x = k0 * t_a_x + x_a_0;
    int_y = k0 * t_a_y + y_a_0;
}

class Point {
public:
    double x = 0, y = 0;
    Point();
    Point(double x, double y);
};

class Intersection {
public:
    Intersection();
    Intersection(double k, int line0, int line);
    bool operator<(const Intersection& b) const;
    double k = 0;
    int line0 = -1;
    int line = -1;
};

class Line {
public:
    Point p1, p2;
    std::set<Intersection> inters;
    void read();
};

std::vector<Line> lines;
std::vector<Intersection> ints;

void read() {
    int n;
    std::cin >> n;
    lines.resize(n);
    for(int i = 0; i < n; i++) {
        lines[i].read();
    }
}

void intersect() {
    for(int i = 0; i < lines.size(); i++) {
        for(int j = 0; j < lines.size(); j++) {
            double int_x, int_y, detA, k0;
            intersect(lines[i].p1.x, lines[i].p1.y, lines[i].p2.x, lines[i].p2.y,
                      lines[j].p1.x, lines[j].p1.y, lines[j].p2.x, lines[j].p2.y,
                      int_x, int_y, detA, k0);
            if(fabs(detA) >= 1e-3) {
                auto in = Intersection(k0, i, j);
                lines[i].inters.insert(in);
                ints.push_back(in);
            }
        }
    }
}

void process() {
    for(int i = 0; i < ints.size(); i++) {
        for(int j = 0; j < ints.size(); j++) {
            if(ints[i].line0 == ints[j].line0) {

            }
        }
    }
}

int main()
{
    read();
    intersect();
    process();

    return 0;
}


Intersection::Intersection(double k, int line0, int line) : k(k), line0(line0), line(line)
{}

bool Intersection::operator<(const Intersection &b) const
{
    return k < b.k;
}

Point::Point()
{}

Point::Point(double x, double y) : x(x), y(y)
{}

void Line::read()
{
    std::cin >> p1.x >> p1.y >> p2.x >> p2.y;
}
