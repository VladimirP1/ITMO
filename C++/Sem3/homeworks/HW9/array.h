#pragma once
#include <utility>
#include <iostream>
#include <stdexcept>
using size_t = unsigned long;

template<class ElementType>
class Array
{
public:
    explicit Array(size_t size = 0);
    Array(const Array& from);
    Array(Array&& from);

    Array& operator=(const Array& from);
    Array& operator=(Array&& from);

    size_t size() const;
    ElementType& operator[](size_t i);
    const ElementType& operator[](size_t i) const;

    ~Array();
private:
    size_t mSize;
    ElementType* mData;

    void allocate_array(size_t size);
    void free_array();
};

#include "array_impl.hpp"
