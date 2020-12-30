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

# -- This project allows us to output some defines that get updated when needed by triggering a re-config from cmake.
nxa_project(NxABuildDefines)

file(WRITE ${CMAKE_BINARY_DIR}/nxa_build_defines.h.in
     "\#define NXA_GIT_COMMIT_HASH \"@NXA_CMAKE_GIT_COMMIT_HASH@\"\n"
     "\#define NXA_GIT_BRANCH \"@NXA_CMAKE_GIT_BRANCH@\"\n"
     "\#define @NXA_BUGFIX_BUILD_DEFINE@ 1\n"
     "\#define @NXA_BETA_BUILD_DEFINE@ 1\n")

file(WRITE ${CMAKE_BINARY_DIR}/nxa_build_defines.cmake
     "list(APPEND CMAKE_MODULE_PATH \"${CMAKE_BINARY_DIR}/../cmake\")
      set(NXA_CMAKE_BETA_BUILD ${NXA_CMAKE_BETA_BUILD})
      set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE})
      include(getversions)
      if(\"$\{NXA_CMAKE_GIT_BRANCH\}\" STREQUAL \"bugfix\")
          set(NXA_BUGFIX_BUILD_DEFINE \"NXA_BUGFIX_BUILD\")
      else()
          set(NXA_BUGFIX_BUILD_DEFINE \"NXA_NOT_BUGFIX_BUILD\")
      endif()
      if(\"$\{NXA_CMAKE_GIT_BRANCH\}\" STREQUAL \"release\")
          set(NXA_BETA_BUILD_DEFINE \"NXA_NOT_BETA_BUILD\")
      elseif(\"$\{NXA_CMAKE_GIT_BRANCH\}\" STREQUAL \"sdk-release\")
          set(NXA_BETA_BUILD_DEFINE \"NXA_NOT_BETA_BUILD\")
      else()
          set(NXA_BETA_BUILD_DEFINE \"NXA_BETA_BUILD\")
      endif()
      configure_file(\${SRC} \${DST} @ONLY)")

add_custom_target(NxABuildDefines ${CMAKE_COMMAND}
                  -D SRC=${CMAKE_BINARY_DIR}/nxa_build_defines.h.in
                  -D DST=${CMAKE_BINARY_DIR}/nxa_build_defines.h
                  -P ${CMAKE_BINARY_DIR}/nxa_build_defines.cmake)

nxa_project_end("NO_EXTRA_COMPILE_DEFINITIONS")

set(SOURCE_FILES
    ${NXA_VENDOR_DIR}/utf8rewind/source/utf8rewind.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/unicodedatabase.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/casemapping.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/codepoint.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/composition.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/database.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/decomposition.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/seeking.c
    ${NXA_VENDOR_DIR}/utf8rewind/source/internal/streaming.c
    Source/Assert.cpp
    Source/Blob.cpp
    Source/Date.cpp
    Source/DecimalNumber.cpp
    Source/Directory.cpp
    Source/Directory${NXA_CMAKE_PLATFORM_PATH_POSTFIX}
    Source/File.cpp
    Source/File${NXA_CMAKE_PLATFORM_PATH_POSTFIX}
    Source/FilePath.cpp
    Source/FilePath${NXA_CMAKE_PLATFORM_PATH_POSTFIX}
    Source/Platform.cpp
    Source/Platform${NXA_CMAKE_PLATFORM_PATH_POSTFIX}
    Source/String.cpp
    Source/Threading.cpp
    Source/Time.cpp
    Source/UserPreferences.cpp
    Source/UserPreferences${NXA_CMAKE_PLATFORM}
    Source/Volume.cpp
    Source/Volume${NXA_CMAKE_PLATFORM_PATH_POSTFIX})

if(NOT ${NXA_CMAKE_PLATFORM} STREQUAL "Linux")
    # -- We don't have HTTPRequest on linux yet.
    set(SOURCE_FILES
        ${SOURCE_FILES}
        Source/EventLog.cpp
        Source/HttpRequest${NXA_CMAKE_PLATFORM_PATH_POSTFIX})
endif()

nxa_project(Base)

if(NOT ${NXA_CMAKE_PLATFORM} STREQUAL "Linux")
    if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        set(NXA_CRASHLOG_IMPLEMENTATION CrashLogMacOSObjectiveC.m)
    endif()

    add_library(Base STATIC
                ${SOURCE_FILES}
                Source/CrashLog${NXA_CMAKE_PLATFORM_PATH_POSTFIX}
                Source/${NXA_CRASHLOG_IMPLEMENTATION}
                Source/XMLDocument.cpp
                Source/XMLNode.cpp)

    # -- TODO: include path for project so only public headers of Base are visible
    target_include_directories(Base
                               PRIVATE ${NXA_VENDOR_DIR}/utf8rewind/include ${NXA_VENDOR_DIR}/utf8rewind/include/utf8rewind
                               PUBLIC Include ${NXA_VENDOR_DIR} ${NXA_VENDOR_DIR}/json/single_include)

    target_link_libraries(Base PUBLIC ${NXA_TESTING_FRAMEWORK_LIBS})

    nxa_depends_on_build_defines(Base NxABuildDefines)

    if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        # -- Bugsnag requires the use of @import
        target_compile_options(Base PRIVATE -fmodules)

        if("${NXA_BUGSNAG_DIR}" STREQUAL "")
            message(FATAL_ERROR "NXA_BUGSNAG_DIR is not defined. Must point to your bugsnag build folder install.")
        endif()

        find_library(BugsnagFramework Bugsnag "${NXA_BUGSNAG_DIR}" NO_DEFAULT_PATH)
        mark_as_advanced(BugsnagFramework)
        target_link_libraries(Base PRIVATE ${BugsnagFramework})

        if (NOT BugsnagFramework)
            message(FATAL_ERROR "BugsnagFramework required, but not found!")
        endif()

        message(STATUS "Bugsnag Framework found at ${BugsnagFramework}.")

        find_library(COCOA_LIBRARY Cocoa)
        find_library(IOKIT_LIBRARY IOKit)
        target_link_libraries(Base PUBLIC ${NXA_TESTING_FRAMEWORK_LIBS} ${COCOA_LIBRARY} ${IOKIT_LIBRARY})
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        target_link_libraries(Base PUBLIC ${NXA_TESTING_FRAMEWORK_LIBS} USER32.LIB Shlwapi.lib Winhttp.lib)

        target_uses_dynamic_runtime(Base)
    endif()
endif()

if(NXA_CMAKE_BUILD_FOR_TESTING)
    add_subdirectory(Tests)
endif()

nxa_project_end()
