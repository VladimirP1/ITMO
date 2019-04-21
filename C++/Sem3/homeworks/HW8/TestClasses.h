#pragma once

struct test
{
    int a;
    int b;
    void z(){}
};

struct Base{ virtual void t(){}};
struct L : Base{L():Base(){}};
struct R : Base{R():Base(){}};
struct Top : L, R{Top():L(), R(){}};
