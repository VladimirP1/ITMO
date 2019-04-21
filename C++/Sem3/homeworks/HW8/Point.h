#ifndef POINT_H
#define POINT_H

class Point
{
public:
    Point(int x, int y);
    double distance(const Point& point) const;
    Point operator+ (const Point& point) const;
    Point& operator+= (const Point& point);
    bool operator== (const Point& point) const;

    int getX() const;
    int getY() const;
private:
    int x, y;
};

#endif // POINT_H
