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

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS" OR ${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    nxa_project(Collections)
    
    add_library(Collections
                Objects/Collections.cpp)

    add_subdirectory(Common)
endif()

add_subdirectory(RekordBuddy)

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS" OR ${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    add_subdirectory(TrackFiles)
    add_subdirectory(Serato)
    add_subdirectory(Rekordbox)
    add_subdirectory(Traktor)
    add_subdirectory(AppleMusic)
    add_subdirectory(Engine)
    add_subdirectory(PCDJ)

    target_include_directories(Collections
                               PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/Include
                               PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/Objects)
    target_link_libraries(Collections PUBLIC Base
                          CommonCollection RekordBuddyCollection SeratoCollection RekordboxCollection TraktorCollection
                          TrackFilesCollection AppleMusicCollection PCDJCollection)

    nxa_project_end()
endif()
