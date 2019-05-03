cmake_minimum_required(VERSION 3.13)
project(tomita)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0")

include_directories(${CMAKE_SOURCE_DIR})

add_subdirectory(googletest)
add_executable(tomita main.cpp LRParser.cpp LRParser.h)

add_executable(lrparser_test lrparser_ut.cpp LRParser.cpp)
target_link_libraries(lrparser_test gtest_main)