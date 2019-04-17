SOURCES=$(wildcard */*/*.cpp)
BINARIES=$(patsubst %.cpp,%.bin,$(SOURCES))

all: bin
bin: $(BINARIES)

%.bin: %.cpp
	g++ -std=c++11 -o $@ $<

.PHONY: all bin 
