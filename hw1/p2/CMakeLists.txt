project(Problem2)
cmake_minimum_required(VERSION 2.8)

set(SRC_LIST
    Problem2.cpp
    main.cpp)

find_package(OpenMP)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")

add_executable(${PROJECT_NAME} ${SRC_LIST})
