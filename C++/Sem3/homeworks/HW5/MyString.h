#pragma once 
#include <cstring>
#include <ostream>

class MyString {
public:
    MyString();
	MyString(const char* data);
	MyString(std::size_t length, char fillChar);
	
	MyString(const MyString& from);
	MyString(MyString&& from);
	MyString& operator=(const MyString& from);
	MyString& operator=(MyString&& from);
	
	MyString& append(const MyString& suffix);
	MyString& reverse();
	
	std::size_t length() const;
	const char* c_str() const;
	
	~MyString();
private:
    MyString(const char* data, std::size_t length);

    char* mData = 0;
    std::size_t mLength = 0;
    
};

std::ostream& operator<< (std::ostream& out, const MyString& string);
