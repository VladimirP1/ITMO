cmake_minimum_required(VERSION 3.1)

project(RationalLib)

set(HEADERS lib_rational.h)
set(SOURCES lib_rational.cpp)

option(BUILD_STATIC_LIBS "Build static library" ON)
option(BUILD_SHARED_LIBS "Build shared library" ON)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# Build
add_library(rational ${LIBRARY_TYPE} ${HEADERS} ${SOURCES})

target_include_directories(rational PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}> $<INSTALL_INTERFACE:include>)

target_compile_options(rational INTERFACE "-O3")

set(CMAKE_CXX_STANDARD 11)

# Test

enable_testing()

add_subdirectory(test)

# Installation

install(
    TARGETS rational
    EXPORT librational
    RUNTIME DESTINATION bin COMPONENT user
    LIBRARY DESTINATION lib COMPONENT user
    ARCHIVE DESTINATION lib COMPONENT developer
)
install(
    FILES ${HEADERS} DESTINATION include
    COMPONENT developer
)

install(
    FILES librational-config.cmake DESTINATION share
    COMPONENT developer
)

install(EXPORT librational DESTINATION share COMPONENT developer)
