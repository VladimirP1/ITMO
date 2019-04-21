#pragma once
#include <utility>
#include <iostream>
#include <stdexcept>
using size_t = unsigned long;

template<class T>
class Array
{
public:
    Array();
    explicit Array(size_t size, const T& value = T());
    Array(const Array& from);
    Array(Array&& from);

    Array& operator=(const Array& from);
    Array& operator=(Array&& from);

    size_t size() const;
    T& operator[](size_t i);
    const T& operator[](size_t i) const;

    ~Array();
private:
    size_t mSize;
    T* mData;

    void allocate_array(size_t size);
};

#include "array_impl.hpp"
