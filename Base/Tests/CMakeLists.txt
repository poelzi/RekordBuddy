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

nxa_project(TestBase)

enable_testing()

set(NXA_TEST_SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/ArrayTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/BlobTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ColorTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/DateTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/DecimalNumberTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/DirectoryTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/DurationTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/FilePathTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/FlagsTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ListTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/MapTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/MutableStringTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/MutableXMLNodeTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/PlatformTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/SetTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/StringTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/TimeTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/UserPreferencesTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/VolumeTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/XMLDocumentTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/XMLNodeTests.cpp)

add_executable(TestBase ${NXA_TEST_SOURCE_FILES})
add_dependencies(TestBase gtest)
target_link_libraries(TestBase PRIVATE Base)

add_test(NAME TestBase COMMAND TestBase)

nxa_project_end()
