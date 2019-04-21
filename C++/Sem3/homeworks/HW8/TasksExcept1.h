#pragma once

typedef int  (*ParamFunction)(double);
typedef int* (*ReturnedFunction)(char const*);
typedef ReturnedFunction (*ComplexFunction)(int, ParamFunction);

template<class T, class R>
bool compare(const T& a, const T& b, R (T::*size)(void) const)
{
    return (a.*size)() < (b.*size)();
}

template<class T>
bool isSameObject(T const* p, T const* q)
{
    return dynamic_cast<void const *>(p) == dynamic_cast<void const *>(q);
}

