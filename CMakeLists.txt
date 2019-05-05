cmake_minimum_required(VERSION 3.13)
project(tomita)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0")

include_directories(${CMAKE_SOURCE_DIR})

add_subdirectory(googletest)
add_executable(tomita main.cpp grl.cpp grl.h)

add_executable(glr_test glr_ut.cpp grl.cpp)
target_link_libraries(glr_test gtest_main)