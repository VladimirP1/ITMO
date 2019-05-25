cmake_minimum_required(VERSION 3.14)
project(Maths)

set(CMAKE_CXX_STANDARD 14)


add_executable(A A.cpp)
add_executable(B B.cpp)
add_executable(D D.cpp)
add_executable(E E.cpp)

add_custom_target(
    F 
    ALL
    COMMAND ghc -o F ${CMAKE_CURRENT_SOURCE_DIR}/F.hs
    DEPENDS 
    ${CMAKE_CURRENT_SOURCE_DIR}/F.hs
)
