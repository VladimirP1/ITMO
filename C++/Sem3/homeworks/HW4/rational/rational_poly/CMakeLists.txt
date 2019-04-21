cmake_minimum_required(VERSION 3.1)

project(RationalPoly)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

if(NOT TARGET rational)
    find_package(librational REQUIRED)
endif()
if(NOT TARGET poly)
    find_package(libpoly REQUIRED)
endif()

add_executable(rational_poly rational_poly.cpp)

install(TARGETS rational_poly COMPONENT user RUNTIME DESTINATION bin)

target_link_libraries(rational_poly rational poly)
