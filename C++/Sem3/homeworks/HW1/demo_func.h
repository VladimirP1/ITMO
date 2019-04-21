// Functions have to be declared before being called
void demo01();
void demo02();

void demo01() {
    demo02();
}

void demo02() {
    demo01();
    // demo03(); // Will fail: demo03 was not declared in this scope
}

void demo03() {

}

// Defining a function automatically declares it:
// writing :
void demo04();
void demo04() {

}
// is equivalent to:
//     void demo04() {
//
//     }

