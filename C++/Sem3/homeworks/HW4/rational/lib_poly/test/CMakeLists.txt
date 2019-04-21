add_executable(polyTest polyTest.cpp)

target_link_libraries(polyTest poly)

add_test(NAME polyTest COMMAND polyTest)
