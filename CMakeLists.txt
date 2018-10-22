cmake_minimum_required(VERSION 3.12)
project(Process_Scheduling_Shootout C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -pthread")

add_executable(Process_Scheduling_Shootout
        shootout.c)
