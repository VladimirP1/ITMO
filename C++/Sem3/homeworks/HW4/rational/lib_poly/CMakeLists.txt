
cmake_minimum_required(VERSION 3.1)

project(PolyLib)

set(HEADERS lib_poly.h)
set(SOURCES lib_poly.cpp)

option(BUILD_STATIC_LIBS "Build static library" ON)
option(BUILD_SHARED_LIBS "Build shared library" ON)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# Build
if(NOT TARGET rational)
    find_package(librational REQUIRED)
endif()

add_library(poly ${LIBRARY_TYPE} ${HEADERS} ${SOURCES})

target_include_directories(poly PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}> $<INSTALL_INTERFACE:include>)

target_link_libraries(poly rational)

set(CMAKE_CXX_STANDARD 11)

# Test

enable_testing()

add_subdirectory(test)

# Installation

# A typical user needs only shared libs
install(
    TARGETS poly
    EXPORT libpoly
    RUNTIME DESTINATION bin COMPONENT user
    LIBRARY DESTINATION lib COMPONENT user
    ARCHIVE DESTINATION lib COMPONENT developer
)

install(
    FILES ${HEADERS} DESTINATION include
    COMPONENT developer
)

install(
    FILES libpoly-config.cmake DESTINATION share
    COMPONENT developer
)

install(EXPORT libpoly DESTINATION share COMPONENT developer)
