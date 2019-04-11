SOURCES=$(wildcard */*/*.cpp)
BINARIES=$(patsubst %.cpp,%.bin,$(SOURCES))

all: bin
bin: $(BINARIES)

%.bin: %.cpp
	g++ -o $@ $<

.PHONY: all bin 
