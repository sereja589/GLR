set(
    SOURCE_FILES
    atn.h
    atn.cpp
        converter_to_grammar.h converter_to_grammar.cpp)

add_library(atn ${SOURCE_FILES})
target_link_libraries(atn ${Boost_LIBRARIES})