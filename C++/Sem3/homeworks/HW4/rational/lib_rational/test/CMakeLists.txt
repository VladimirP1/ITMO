add_executable(rationalTest rationalTest.cpp)

target_link_libraries(rationalTest rational)

add_test(NAME rationalTest COMMAND rationalTest)
