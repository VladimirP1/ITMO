// Classes and objects


struct AbstractWriter {
    void demo_showMyName() {
        cerr << "AbstractWriter" << endl;
    }
    virtual void write(string s) = 0;
    virtual void writeln(string s) {
        write(s + "\n");
    }
    virtual ~AbstractWriter() { }
};

struct FileWriter : public AbstractWriter {
    void demo_showMyName() {
        cerr << "FileWriter" << endl;
    }
    virtual void write(string s) {
        stream << s;
    }
    FileWriter(string filename) : stream(filename) {}
    virtual ~FileWriter() { stream.close(); }
private:
    ofstream stream;
};

struct FileWriterWindows : public FileWriter {
    using FileWriter::FileWriter;
    void demo_showMyName() {
        cerr << "FileWriterWindows" << endl;
    }
    virtual void writeln(string s) override {
        write(s + "\r\n");
    }
};

struct ConsoleWriter : public AbstractWriter {
    void demo_showMyName() {
        cerr << "ConsoleWriter" << endl;
    }
    virtual void write(string s) {
        cout << s;
    }
};


void demo_write(AbstractWriter& writer) {
    writer.write("test 1 ");
    writer.writeln("test 2");
    writer.writeln("test 3");
    writer.demo_showMyName();
}

void demo_classes_objects() {
    // Creating objects
    FileWriter demo1 ("/tmp/demo2");
    FileWriter *demo2 = new FileWriter("/tmp/demo2");

    // Demo2 will not be destroyed after going out of scope.
    // Destroy it.
    delete demo2;

    //AbstractWriter wr1; // Will not compile: has pure virtual functions
    FileWriter wr2("/tmp/test");
    FileWriterWindows wr3("/tmp/test.w");
    ConsoleWriter wr4;
    demo_write(wr2);
    demo_write(wr3);
    demo_write(wr4);
}

// Constructors
class ConstructorDemo {
    int* demo_array;
public:
    // Constructors, like member functions, can be overloaded
    ConstructorDemo() : demo_array(nullptr) {}
    ConstructorDemo(size_t sz) : demo_array(new int[sz]()) {}
    // Stub copy constructor
    ConstructorDemo(const ConstructorDemo& cd) = delete;
    // Move constructor
    ConstructorDemo(ConstructorDemo&& cd) {
        cout << "Move" << endl;
        demo_array = 0;
        swap(demo_array, cd.demo_array);
    }

    // A simple destructor
    ~ConstructorDemo() {
        if(demo_array) delete demo_array;
        cout << "destroyed" << endl;
    }
protected:
    // We can have a protected constructor, but not a private
    ConstructorDemo(int a, int b) : demo_array(nullptr) {
        cout << "This is a protected constructor " << a << " " << b << endl;
    }
};

struct ConstructorDemoSubclass : public ConstructorDemo {
    ConstructorDemoSubclass() : ConstructorDemo(1, 2) {}
};

void demo_constructors() {
    ConstructorDemo demo1;
    ConstructorDemo demo2(10);
    ConstructorDemoSubclass demo3;

    ConstructorDemo demo4 = std::move(demo2);
    // Won't work, we have deleted the copy constructor
    // ConstructorDemo demo5 = demo1;

}

// Pass by value/reference, return by reference
template <typename T>
void my_swap(T a, T b) {
    T tmp = move(a);
    a = move(b);
    b = move(tmp);
}

void func_valpass(int a) {
    a++;
}

void func_refpass(int& a) {
    a++;
}

//void func_constrefpass(const int& a) {
    //a ++; // Will not compile
//}

// WHAT NOT to do:
// We return a local variable and it gets destroyed
// even before the control is given back to the caller
//
//     ConsoleWriter& construct_writer() {
//         ConsoleWriter writer;
//         return writer;
//     }

// The better way:
ConsoleWriter& construct_writer() {
    return * new ConsoleWriter();
}

// The major drawback of such construction is that
// we will have to delete the object we got a reference to:
void demo_construct_writer_drawback() {
    ConsoleWriter& w = construct_writer();
    delete &w;

    // If we write like that, we get a memory leak, because
    // the returned object is copied to w1, which is allocated on stack,
    // and is left behind in ram

    //ConsoleWriter w1 = construct_writer();
}

void demo_value_reference() {
    int a = 0;
    func_valpass(a);
    cout << a << endl;
    func_refpass(a);
    cout << a << endl;

    demo_construct_writer_drawback();
}

// Default arguments
int demo_defarg(int a, int b, int c = 0) {
    return a + b + c;
}
// This won't work: we still need to specify c, so giving b a default value seems unlogical.
// The compiler detects it and gives an error;
//
//     int demo_defarg(int a, int b = 0, int c) {
//         return a + b + c;
//     }

// Function overloading
int sum(int a, int b) {
    cout << "int" << endl;
    return a + b;
}

double sum(double a, double b) {
    cout << "double" << endl;
    return a + b;
}

double ident(int a) {
    cout << "int" << endl;
    return a;
}

double ident(uint8_t a) {
    cout << "int" << endl;
    return a;
}

double ident(double a) {
    cout << "double" << endl;
    return a;
}

// Functions
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
//     void demo04();
//     void demo04() {
//
//     }
// is equivalent to:
//     void demo04() {
//
//     }

// Inlining, does not guarantee anything
// Only recommends the compiler to copy-paste the code
// into the place where it is called from.
// In fact, with -O0 flag, this function does not get inlined.
inline int inl(int a) {
    return a + 1;
}


void demo() {
    cout << "---- Classes/objects ----" << endl;
    demo_classes_objects();

    cout << "---- Constructors ----" << endl;
    demo_constructors();

    cout << "---- Passing/returning by value/reference ----" << endl;
    demo_value_reference();

    cout << "---- Default args ----" << endl;
    demo_defarg(1, 1);
    demo_defarg(1, 1, 1);

    cout << "---- Function Overloading ----" << endl;
    sum(0, 1);
    sum(0.5, 1.0);
    ident(2.0);
    ident(2.0F);
    ident(2);
    ident((uint16_t) 2); // Widening is preferred

    cout << "---- Functions ----" << endl;
    cout << inl(1) << endl;

    //

    int a = 0, b = 1;
    swap(a, b);

}
