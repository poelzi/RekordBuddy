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

include(version)

# -- Get some information on the current state of our local git repo.
execute_process(COMMAND git symbolic-ref --short HEAD
                WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                OUTPUT_VARIABLE NXA_CMAKE_GIT_BRANCH
                OUTPUT_STRIP_TRAILING_WHITESPACE)

if ("${NXA_CMAKE_GIT_BRANCH}" STREQUAL "")
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(NXA_CMAKE_GIT_BRANCH "UNKNOWN")
    else()
        message(FATAL_ERROR "Couldn't find git branch (missing git executable?).")
    endif()
endif()

execute_process(COMMAND git log -1 --format=%h
                WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                OUTPUT_VARIABLE NXA_CMAKE_GIT_COMMIT_HASH
                OUTPUT_STRIP_TRAILING_WHITESPACE)

if ("${NXA_CMAKE_GIT_COMMIT_HASH}" STREQUAL "")
    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(NXA_CMAKE_GIT_COMMIT_HASH "UNKNOWN")
    else()
        message(FATAL_ERROR "Couldn't find git commit hash (missing git executable?).")
    endif()
endif()

string(SUBSTRING ${NXA_CMAKE_GIT_COMMIT_HASH} 0 8 NXA_CMAKE_GIT_COMMIT_HASH)

if("${NXA_CMAKE_GIT_BRANCH}" STREQUAL "bugfix")
    set(NXA_BUGFIX_BUILD_DEFINE "NXA_BUGFIX_BUILD")
else()
    set(NXA_BUGFIX_BUILD_DEFINE "NXA_NOT_BUGFIX_BUILD")
endif()

if("${NXA_CMAKE_GIT_BRANCH}" STREQUAL "release")
    if(NXA_CMAKE_BETA_BUILD)
        message(FATAL_ERROR "Should not be building beta builds in this branch.")
    endif()
else()
    if(NOT NXA_CMAKE_BETA_BUILD)
        message(FATAL_ERROR "Should only be building beta builds in this branch.")
    endif()
endif()

# -- Set up our project version number string. This needs to be used everywhere possible for consistency.
if("${RKB_CMAKE_REVISION_VERSION}" STREQUAL "0")
    set(RKB_CMAKE_VERSION_NUMBER "${RKB_CMAKE_MAJOR_VERSION}.${RKB_CMAKE_MINOR_VERSION}")
else()
    set(RKB_CMAKE_VERSION_NUMBER "${RKB_CMAKE_MAJOR_VERSION}.${RKB_CMAKE_MINOR_VERSION}.${RKB_CMAKE_REVISION_VERSION}")
endif()

# -- Package version must be major.minor[.revision] without any beta indicators or "v" prefix.
set(RKB_CMAKE_PACKAGE_VERSION "${RKB_CMAKE_VERSION_NUMBER}")

# -- If we're not in the release branch then this is a beta build.
if(NXA_CMAKE_BETA_BUILD)
    set(RKB_CMAKE_BUNDLE_VERSION "${RKB_CMAKE_MAJOR_VERSION}.${RKB_CMAKE_MINOR_VERSION}.${RKB_CMAKE_REVISION_VERSION}b${RKB_CMAKE_BETA_VERSION}")
    set(RKB_CMAKE_VERSION_STRING "${RKB_CMAKE_VERSION_NUMBER}b${RKB_CMAKE_BETA_VERSION}")
    set(RKB_CMAKE_VERSION_CRASHLOG_STRING "${RKB_CMAKE_VERSION_NUMBER}b${RKB_CMAKE_BETA_VERSION}(${NXA_CMAKE_GIT_COMMIT_HASH})")
    set(RKB_CMAKE_SYMBOLS_VERSION_STRING "${RKB_CMAKE_VERSION_NUMBER}.b${RKB_CMAKE_BETA_VERSION}.${NXA_CMAKE_GIT_COMMIT_HASH}")
else()
    set(RKB_CMAKE_BUNDLE_VERSION "${RKB_CMAKE_MAJOR_VERSION}.${RKB_CMAKE_MINOR_VERSION}.${RKB_CMAKE_REVISION_VERSION}")
    set(RKB_CMAKE_VERSION_STRING "${RKB_CMAKE_VERSION_NUMBER}")
    set(RKB_CMAKE_VERSION_CRASHLOG_STRING "${RKB_CMAKE_VERSION_NUMBER}")
    set(RKB_CMAKE_SYMBOLS_VERSION_STRING "${RKB_CMAKE_VERSION_NUMBER}")
endif()
