set(
    SOURCE_FILES
    main.cpp
)

add_executable(lr_table_builder ${SOURCE_FILES})
target_link_libraries(lr_table_builder glr)