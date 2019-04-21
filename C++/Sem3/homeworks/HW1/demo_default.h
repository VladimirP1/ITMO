int demo11(int a, int b, int c = 0) {
    return a + b + c;
}

// Will not compile: there's no point in specifying
   // a default argument for b, because we still have to give c.
//     int demo12(int a, int b = 0, int c) {
//         return a + b + c
//     }


void run_demo1() {
    demo11(1, 2);
    demo11(1, 2, 3);
}
