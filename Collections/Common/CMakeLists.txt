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

set(COMMON_SOURCE_FILES
    Objects/Crates/CratePath.cpp
    Objects/Crates/Folder.cpp
    Objects/Crates/Playlist.cpp
    Objects/Crates/SmartPlaylist.cpp
    Objects/Markers/MarkerValidation.cpp
    Objects/Markers/MarkerColor.cpp
    Objects/Tracks/MusicalKey.cpp
    Objects/Tracks/Property.cpp
    Objects/Tracks/TrackColor.cpp
    Objects/Tracks/TrackDisplayCache.cpp
    Objects/Tracks/TrackPredicate.cpp
    Objects/Tracks/TrackRating.cpp
    Objects/Collection.cpp)

nxa_project(CommonCollection)

add_library(CommonCollection STATIC ${COMMON_SOURCE_FILES}
            Objects/Tracks/Track.cpp
            Objects/Markers/MarkerOffset.cpp)

target_include_directories(CommonCollection
                           PUBLIC Include)

target_link_libraries(CommonCollection PUBLIC TrackFiles Base)

if(NXA_CMAKE_BUILD_FOR_TESTING)
    add_subdirectory(Tests)
endif()

nxa_project_end()
