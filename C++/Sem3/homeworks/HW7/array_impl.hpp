#pragma once
#include "array.h"

template <class T>
Array<T>::Array() : mSize(0), mData(nullptr) {

}

template <class T>
Array<T>::Array(size_t size, const T& value) {
    allocate_array(size);
    while (size--) {
        new (&mData[size]) T(value);
    }
}

template<class T>
Array<T>::Array(const Array<T>& from) : Array(0) {
    *this = from;
}

template<class T>
Array<T>::Array(Array<T>&& from) : Array(0)
{
    *this = std::move(from);
}

template<class T>
Array<T>& Array<T>::operator=(const Array& from) {
    *this = std::move(Array<T>());
    allocate_array(from.size());
    for (int i = 0; i < from.size(); i++) {
        new (&mData[i]) T(from.mData[i]);
    }
    return *this;
}

template<class T>
Array<T>& Array<T>::operator=(Array&& from){
    std::swap(from.mSize, mSize);
    std::swap(from.mData, mData);
    return *this;
}

template<class T>
size_t Array<T>::size() const
{
    return mSize;
}

template<class T>
T &Array<T>::operator[](size_t i)
{
    return *(mData + i);
}

template<class T>
const T &Array<T>::operator[](size_t i) const
{
    return *(mData + i);
}

template<class T>
Array<T>::~Array()
{
    if (mSize) {
        while (mSize--) {
            mData[mSize].~T();
        }
        delete[] (reinterpret_cast<char*>(mData));
    }
}

template<class T>
void flatten(const T& element, std::ostream& out) {
        out << element << " ";
}

template<class T>
void flatten(const Array<T>& array, std::ostream& out) {
    for (size_t i = 0 ; i < array.size(); i++) {
        flatten(array[i], out);
    }
}

template<class T, class L>
T minimum(Array<T> array, L less) {
    if(array.size() == 0) {
        throw std::invalid_argument("Error: Taking minimum of an empty array");
    }
    T min = array[0];
    size_t minIndex = 0;
    for (size_t i = 0 ; i < array.size(); i++) {
        if (less(array[i], min)) {
            min = array[i];
            minIndex = i;
        }
    }
    return array[minIndex];
}

template<class T>
void Array<T>::allocate_array(size_t size) {
    mSize = size;
    mData = mSize > 0 ? reinterpret_cast<T*>(new char[mSize * sizeof(T)]) : nullptr;
}
