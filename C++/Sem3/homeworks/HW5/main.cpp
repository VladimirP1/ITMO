#include "MyString.h"
#include <iostream>
#include <array>

 int main() {
	 std::array<MyString, 10> strings;
	 for(int i = 0; i < 100000; i++) {
		 int s1 = rand() % strings.size();
		 int s2 = rand() % strings.size();
		 int op = rand() % 8;
		 
		 switch(op) {
			 // Test all constructors
			 case 0:
			     strings[s1].~MyString();
			     new (&strings[s1]) MyString();
			 break;
			 case 1:
			 	 strings[s1].~MyString();
			     new (&strings[s1]) MyString(" test ");
			 break;
			 case 2:
			     strings[s1].~MyString();
			     new (&strings[s1]) MyString(5, 'a');
			 break;
			 case 3:
			     if(s1 != s2) {
			         strings[s1].~MyString();
			         new (&strings[s1]) MyString(strings[s2]);
				 }
			 break;
			 case 4:
			 	 if(s1 != s2) {
			         strings[s1].~MyString();
			         new (&strings[s1]) MyString(std::move(strings[s2]));
				 }
			 break;
			 // Assignments
			 case 5:
			     strings[s1] = strings[s2];
			 break;
			 case 6:
			     strings[s1] = std::move(strings[s2]);
			 break;
			 // Append
			 case 7 :
			     strings[s1].append(strings[s2]);
			     strings[s2].reverse();
			 break;
			 case 8:
			     std::cout << strings[s1] << std::endl;
		 }
	 }
	 
	 MyString s1("Hello");
	 MyString s2(" world.");
	 s1.append(s2);
	 std::cout << s1 << std::endl;
	 
	 MyString s("Hello");
	 s.append(s);
	 std::cout << s << std::endl;
	 
	 MyString t("tS");
	 t.append("").reverse().append("seT").reverse().append("tring").reverse().append("yM").reverse();
	 std::cout << t << std::endl;
	 
	 
 }
