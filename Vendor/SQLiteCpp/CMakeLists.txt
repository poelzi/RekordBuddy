#
#  Rekord Buddy - The future proof music collection tool made by DJs for DJs.
#  Copyright (C) 2020-2021 Didier Malenfant (didier@rekordbuddy.org)
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

cmake_minimum_required(VERSION 3.13)

nxa_project(SQLiteCpp)

add_library(SQLiteCpp STATIC
            src/sqlite3.c
            src/Column.cpp
            src/Database.cpp
            src/Statement.cpp
            src/Transaction.cpp)

target_compile_definitions(SQLiteCpp PRIVATE -DSQLITE_TEMP_STORE=2)

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
    target_compile_definitions(SQLiteCpp PRIVATE -DSQLITE_OS_UNIX=1 -DSQLITE_THREADSAFE=1)

    find_library(SecurityFramework Security)
    target_link_libraries(SQLiteCpp  PUBLIC ${SecurityFramework})
elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    target_compile_definitions(SQLiteCpp PRIVATE -DNXA_SKIP_COCOA_PROFILING_SIGNPOSTS -DSQLITE_OS_WIN=1 -DBUILD_sqlite -DNDEBUG -DSQLITE_THREADSAFE=1)
else()
    message(FATAL_ERROR "Unsupported platform.")
endif()

target_compile_options(SQLiteCpp PRIVATE -Wno-error)
target_compile_options(SQLiteCpp PRIVATE -Wno-deprecated-declarations)
target_compile_options(SQLiteCpp PRIVATE -Wno-sign-compare)
target_compile_options(SQLiteCpp PRIVATE -Wno-unused-function)
target_compile_options(SQLiteCpp PRIVATE -Wno-unused-variable)
# - sqlite3 headers use old-style cast, but SQLiteCpp needs those headers in the private implementation
target_compile_options(SQLiteCpp PRIVATE -Wno-old-style-cast)

target_uses_dynamic_runtime(SQLiteCpp)

target_include_directories(SQLiteCpp PRIVATE private_include PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_include_directories(SQLiteCpp PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

nxa_project_end()
