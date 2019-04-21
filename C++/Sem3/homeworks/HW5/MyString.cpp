#include "MyString.h"
#include <algorithm>

MyString::MyString() : MyString("") {}

MyString::MyString(const char* data) : MyString(data, strlen(data)) {}

MyString::MyString(std::size_t length, char fillChar) {
	mLength = length;
	mData = new char[mLength + 1];
	std::fill_n(mData, mLength, fillChar);
	mData[mLength] = 0;
}

MyString::MyString(const MyString& from) : MyString(from.c_str(), from.length()) {}

MyString::MyString(MyString&& from) : MyString() {
	std::swap(mLength, from.mLength);
	std::swap(mData, from.mData);
}

MyString& MyString::operator=(const MyString& from) {
	(*this) = std::move(MyString(from));
	return *this;
}

MyString& MyString::operator=(MyString&& from) {
	std::swap(mLength, from.mLength);
	std::swap(mData, from.mData);
	return *this;
}

MyString& MyString::append(const MyString& suffix) {
	std::size_t newLength = mLength + suffix.length();
	char* newData = new char[newLength + 1];
	
	std::copy_n(mData, mLength, newData);
    std::copy_n(suffix.c_str(), suffix.length() + 1, newData + mLength);

	
	delete[] mData;
	mData = newData;
	mLength = newLength;
	
	return *this;
}

MyString& MyString::reverse() {
	std::reverse(mData, mData + mLength);
	return *this;
}

std::size_t MyString::length() const {
	return mLength;
}

const char* MyString::c_str() const {
	return mData;
}

MyString::~MyString() {
	delete[] mData;
}

MyString::MyString(const char* data, std::size_t length) {
    mLength = length;
    mData = new char[mLength + 1];
    std::copy_n(data, mLength + 1, mData);
}

std::ostream& operator<< (std::ostream& out, const MyString& string) {
	out << string.c_str();
}

