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

set(NXA_TRACKFILES_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(NXA_TRACKFILES_DIR ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE)
set(NXA_TRACKFILES_DIR ${CMAKE_CURRENT_SOURCE_DIR})

nxa_project(TrackFiles)

add_library(TrackFiles STATIC
            Objects/SeratoMarkers/SeratoCueMarker.cpp
            Objects/SeratoMarkers/SeratoLoopMarker.cpp
            Objects/SeratoMarkers/SeratoGridMarker.cpp
            Objects/SeratoMarkers/SeratoMarker.cpp
            Objects/AIFFTrackFile.cpp
            Objects/FLACTrackFile.cpp
            Objects/MP4TrackFile.cpp
            Objects/MPEGTrackFile.cpp
            Objects/OGGTrackFile.cpp
            Objects/TrackFile.cpp
            Objects/AudioFileType.cpp
            Objects/TrackFileFactory.cpp
            Objects/WAVTrackFile.cpp)

target_include_directories(TrackFiles
                           PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/Include
                           PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/Objects)
target_link_libraries(TrackFiles PUBLIC Base PRIVATE TagLib)

if(NXA_CMAKE_BUILD_FOR_TESTING)
    add_subdirectory(Tests)
endif()

nxa_project_end()
