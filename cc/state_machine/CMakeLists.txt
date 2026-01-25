cmake_minimum_required(VERSION 3.16)
project(state_machine LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(EXECUTABLE_NAME state_machine)
set(LYBRARY_NAME state_machine_door)

add_subdirectory(app)
add_subdirectory(src)