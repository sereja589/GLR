set(
   SOURCE_FILES
   grammar.h
   grammar.cpp
   glr.h
   glr.cpp
   glr_processor.h
   glr_processor.cpp
   glr_tables.h
   glr_tables.cpp
)

add_library(glr ${SOURCE_FILES})

set(
    TEST_SOURCE_FILES
    ut/glr_ut.cpp
)

add_executable(glr_test ${TEST_SOURCE_FILES} ${SOURCE_FILES})
target_link_libraries(glr_test gtest_main)