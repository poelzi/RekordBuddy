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

nxa_project(TestTraktorCollection)

enable_testing()

set(NXA_TEST_SOURCE_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/Tracks/TrackTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/Crates/PlaylistTests.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/Crates/FolderTests.cpp)

add_executable(TestTraktorCollection ${NXA_TEST_SOURCE_FILES})
add_dependencies(TestTraktorCollection gtest)
target_link_libraries(TestTraktorCollection PRIVATE Base TraktorCollection)

inject_all_binaries_for(TestTraktorCollection)

add_test(NAME TestTraktorCollection
         COMMAND TestTraktorCollection)

nxa_project_end()
