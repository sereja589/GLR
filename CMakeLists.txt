cmake_minimum_required(VERSION 3.13)
project(tomita)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -fsanitize=address")

include_directories(${CMAKE_SOURCE_DIR})

find_package(Boost 1.72.0 REQUIRED COMPONENTS)
include_directories(${Boost_INCLUDE_DIRS})

add_subdirectory(googletest)
add_subdirectory(glr)
add_subdirectory(atn)
# add_subdirectory(morphology)
# add_subdirectory(lexer)
add_subdirectory(lr_table_builder)

set(
    SOURCE_FILES
    main.cpp
)

add_executable(tomita ${SOURCE_FILES})
