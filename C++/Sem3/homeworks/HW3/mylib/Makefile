SOURCES:=hello.cpp here.cpp bye.cpp
LIBNAME:=mylib
MAJOR:=5
MINOR:=1
RELEASE:=10
BUILD_DIR:=build
INSTALL_DIR:=out

LIB_SYMLINK_NAME:=lib$(LIBNAME).so
LIB_SONAME:=$(LIB_SYMLINK_NAME).$(MAJOR)
LIB_FILENAME:=$(LIB_SONAME).$(MINOR).$(RELEASE)

OBJECTS:=$(patsubst %.cpp,$(BUILD_DIR)/%.o,$(SOURCES))

all: libs $(INSTALL_DIR)/main

libs: $(INSTALL_DIR)/$(LIB_FILENAME) $(INSTALL_DIR)/mylib.a

mrproper: clean
	rm -rf $(INSTALL_DIR)

clean:
	rm -rf $(BUILD_DIR)

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(INSTALL_DIR):
	mkdir -p $(INSTALL_DIR)

# Process dependencies on headers
-include $(patsubst %.cpp, $(BUILD_DIR)/%.d, $(SOURCES))

# Build object files
$(BUILD_DIR)/%.o: %.cpp | build
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -MMD -fPIC -c -o $@ $<

.PHONY: mrproper clean all libs

# Build a dynamic library
$(INSTALL_DIR)/$(LIB_FILENAME): $(OBJECTS) | out
	$(CXX) $(LDFLAGS) -Wl,-soname=$(LIB_SONAME) -shared -fPIC -o $@ $^
	ln -sf $(LIB_FILENAME) $(INSTALL_DIR)/$(LIB_SONAME)
	ln -sf $(LIB_FILENAME) $(INSTALL_DIR)/$(LIB_SYMLINK_NAME)

# Build a static library
$(INSTALL_DIR)/mylib.a: $(OBJECTS) | out
	$(AR) cr -o $@ $^

# Build an executable statically linked with the library
$(INSTALL_DIR)/main: $(OBJECTS) $(BUILD_DIR)/main.o | out
	$(CXX) $(LDFLAGS) -o $@ $^

