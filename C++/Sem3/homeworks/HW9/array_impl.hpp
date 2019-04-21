#pragma once
#include "array.h"

template <class ElementType>
Array<ElementType>::Array(size_t size) {
    allocate_array(size);
    while (--size) {
        new (&mData[size]) ElementType();
    }
}

template<class ElementType>
Array<ElementType>::Array(const Array<ElementType>& from) : Array(0) {
    *this = from;
}

template<class ElementType>
Array<ElementType>::Array(Array<ElementType>&& from) : Array(0)
{
    *this = std::move(from);
}

template<class ElementType>
Array<ElementType>& Array<ElementType>::operator=(const Array& from) {
    *this = std::move(Array<ElementType>());
    allocate_array(from.size());
    for (int i = 0; i < from.size(); i++) {
        new (&mData[i]) ElementType(from.mData[i]);
    }
    return *this;
}

template<class ElementType>
Array<ElementType>& Array<ElementType>::operator=(Array&& from){
    std::swap(from.mSize, mSize);
    std::swap(from.mData, mData);
    return *this;
}

template<class ElementType>
size_t Array<ElementType>::size() const
{
    return mSize;
}

template<class ElementType>
ElementType &Array<ElementType>::operator[](size_t i)
{
    return *(mData + i);
}

template<class ElementType>
const ElementType &Array<ElementType>::operator[](size_t i) const
{
    return *(mData + i);
}

template<class ElementType>
Array<ElementType>::~Array()
{
    free_array();
}

template<class ElementType>
void Array<ElementType>::free_array() {
    if (mSize) {
        while (mSize--) {
            mData[mSize].~ElementType();
        }
        delete[] (reinterpret_cast<char*>(mData));
    }
}

template<class ElementType>
void Array<ElementType>::allocate_array(size_t size) {
    mSize = size;
    mData = mSize > 0 ? reinterpret_cast<ElementType*>(new char[mSize * sizeof(ElementType)]) : nullptr;
}

