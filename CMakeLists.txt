cmake_minimum_required(VERSION 3.13)
project(tomita)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Werror")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -fsanitize=address")

include_directories(${CMAKE_SOURCE_DIR})

add_subdirectory(googletest)
add_subdirectory(glr)

set(
    SOURCE_FILES
    main.cpp
)

add_executable(tomita ${SOURCE_FILES})
target_link_libraries(tomita glr)