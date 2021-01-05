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

# -- This is not used but quiets a warning from cmake.
project(RekordBuddyWorkspace)

# -- Special policies

# -- Pull in useful boilerplate and some external settings
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")

set(NXA_VENDOR_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Vendor)
set(NXA_INSTALLER_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Installer)
set(NXA_TOP_PROJECT_DIR ${CMAKE_CURRENT_SOURCE_DIR})

include(getversions)

# -- Set a default build type if none was specified

set(default_build_type "RelWithDebInfo")

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to '${default_build_type}' as none was specified.")

    set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Choose the type of build." FORCE)

    # -- Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo")
endif()

# -- This is required at the root of the top CMAkefile in order for testing to work
if(NXA_CMAKE_BUILD_FOR_TESTING)
    enable_testing()
endif()

# -- Set a variable with the filesystem path component we use for platform-specific stuff

macro(detect_platform)
    if(APPLE)
        set(NXA_CMAKE_PLATFORM macOS)
        set(NXA_CMAKE_PLATFORM_PATH_POSTFIX MacOS.mm)
    elseif(WIN32)
        set(NXA_CMAKE_PLATFORM Windows)
        set(NXA_CMAKE_PLATFORM_PATH_POSTFIX Windows.cpp)
    elseif(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
        set(NXA_CMAKE_PLATFORM Linux)
        set(NXA_CMAKE_PLATFORM_PATH_POSTFIX Linux.cpp)
    else()
        message(FATAL_ERROR "Unsupported platform")
    endif()
endmacro()

macro(add_platform_definitions)
    if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        add_definitions(-DNXA_PLATFORM_MACOS)
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        add_definitions(-DNXA_PLATFORM_WINDOWS)
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Linux")
        add_definitions(-DNXA_PLATFORM_LINUX)
    else()
        message(FATAL_ERROR "Unsupported platform")
    endif()
endmacro()

# -- Set the target SDK/platform for windows builds (it must be before any 'project')

macro(set_system_version)
    if(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        set(CMAKE_SYSTEM_VERSION 10 CACHE STRING INTERNAL FORCE)
    endif()
endmacro()

# -- Macros and functions that we'll reuse across all projects (could be factored out to an include file)

function(print_link_libraries_for_project project_name)
    get_target_property(link_libraries ${project_name} INTERFACE_LINK_LIBRARIES)
    if(link_libraries)
        message(STATUS "-=> Project ${project_name} has interface libraries `${link_libraries}`.")
    endif()
    get_target_property(link_libraries ${project_name} LINK_LIBRARIES)
    if(link_libraries)
        message(STATUS "-=> Project ${project_name} has libraries `${link_libraries}`.")
    endif()
endfunction()

function(print_includes_definitions_and_compile_options_for_project project_name)
    get_target_property(include_directories ${project_name} INTERFACE_INCLUDE_DIRECTORIES)
    if(include_directories)
        message(STATUS "-=> Project ${project_name} has interface includes `${include_directories}`.")
    endif()
    get_target_property(include_directories ${project_name} INCLUDE_DIRECTORIES)
    if(include_directories)
        message(STATUS "-=> Project ${project_name} has includes `${include_directories}`.")
    endif()

    get_target_property(compile_definitions ${project_name} INTERFACE_COMPILE_DEFINITIONS)
    if(compile_definitions)
        message(STATUS "-=> Project ${project_name} has interface compile definitions `${compile_definitions}`.")
    endif()
    get_target_property(compile_definitions ${project_name} COMPILE_DEFINITIONS)
    if(compile_definitions)
        message(STATUS "-=> Project ${project_name} has compile definitions `${compile_definitions}`.")
    endif()

    get_target_property(compile_options ${project_name} INTERFACE_COMPILE_OPTIONS)
    if(compile_options)
        message(STATUS "-=> Project ${project_name} has interface compile options `${compile_options}`.")
    endif()
    get_target_property(compile_options ${project_name} COMPILE_OPTIONS)
    if(compile_options)
        message(STATUS "-=> Project ${project_name} has compile options `${compile_options}`.")
    endif()

    get_target_property(link_libraries ${project_name} INTERFACE_LINK_LIBRARIES)
    if(link_libraries)
        message(STATUS "-=> Project ${project_name} has interface link libraries `${link_libraries}`.")
    endif()
    get_target_property(link_libraries ${project_name} LINK_LIBRARIES)
    if(link_libraries)
        message(STATUS "-=> Project ${project_name} has link libraries `${link_libraries}`.")
    endif()
endfunction()

function(add_includes_definitions_and_compile_options_from_project target_name private_or_public project_name)
    get_target_property(libraries ${project_name} INTERFACE_LINK_LIBRARIES)
    foreach(lib_name ${libraries})
        string(SUBSTRING ${lib_name} 0 1 first_char)
        if(NOT "${first_char}" STREQUAL "$")
            if (TARGET ${lib_name})
                add_includes_definitions_and_compile_options_from_project(${target_name} ${private_or_public} ${lib_name})
            endif()
        endif()
    endforeach()

    get_target_property(include_directories ${project_name} INTERFACE_INCLUDE_DIRECTORIES)
    if(include_directories)
        get_target_property(existing_include_directories ${target_name} INCLUDE_DIRECTORIES)
        if (NOT existing_include_directories)
            set(existing_include_directories "")
        endif()

        foreach(include_path ${include_directories})
            list(APPEND existing_include_directories ${include_path})
        endforeach()

        list(REMOVE_DUPLICATES existing_include_directories)
        set_target_properties(${target_name} PROPERTIES INCLUDE_DIRECTORIES "${existing_include_directories}")
    endif()

    get_target_property(compile_definitions ${project_name} INTERFACE_COMPILE_DEFINITIONS)
    if(compile_definitions)
        get_target_property(existing_compile_definitions ${target_name} COMPILE_DEFINITIONS)
        if (NOT existing_compile_definitions)
            set(existing_compile_definitions "")
        endif()

        foreach(compile_definition ${compile_definitions})
            list(APPEND existing_compile_definitions ${compile_definition})
        endforeach()

        list(REMOVE_DUPLICATES existing_compile_definitions)
        set_target_properties(${target_name} PROPERTIES COMPILE_DEFINITIONS "${existing_compile_definitions}")
    endif()

    get_target_property(compile_options ${project_name} INTERFACE_COMPILE_OPTIONS)
    if(compile_options)
        get_target_property(existing_compile_options ${target_name} COMPILE_OPTIONS)
        if (NOT existing_compile_options)
            set(existing_compile_options "")
        endif()

        foreach(compile_option ${compile_options})
            list(APPEND existing_compile_options ${compile_option})
        endforeach()

        list(REMOVE_DUPLICATES existing_compile_options)
        set_target_properties(${target_name} PROPERTIES COMPILE_OPTIONS "${existing_compile_options}")
    endif()
endfunction()

function(target_uses_static_runtime target_name)
    if(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            target_compile_options(${target_name} PUBLIC /MTd)
        else()
            target_compile_options(${target_name} PUBLIC /MT)
        endif()
    endif()
endfunction()

function(target_uses_dynamic_runtime target_name)
    if(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            target_compile_options(${target_name} PUBLIC /MDd)
        else()
            target_compile_options(${target_name} PUBLIC /MD)
        endif()
    endif()
endfunction()

# Helper function to add preprocesor definition of FILE_BASENAME
# to pass the filename without directory path for debugging use.
#
# Example:
#
#   define_file_basename_for_sources(my_target)
#
# Will add -DFILE_BASENAME="filename" for each source file depended on
# by my_target, where filename is the name of the file.
#
function(define_file_basename_for_sources targetname)
    get_target_property(source_files "${targetname}" SOURCES)
    foreach(sourcefile ${source_files})
        # -- Add the __BASEFILENAME__=filename compile definition to the list.
        get_filename_component(basename "${sourcefile}" NAME)
        # -- Set the updated compile definitions on the source file.
        set_property(SOURCE "${sourcefile}" APPEND
                     PROPERTY COMPILE_DEFINITIONS "__BASEFILENAME__=\"${basename}\"")
    endforeach()
endfunction()

macro(nxa_project argument)
    set(NXA_PROJECT_NAME ${argument})
    set(CMAKE_PROJECT_NAME ${argument})

    set(ARGUMENTS_AS_LIST ${argument} ${ARGN})
    project(${ARGUMENTS_AS_LIST})

    add_platform_definitions()
endmacro()

macro(nxa_project_end)
    if (NOT ${ARGC} OR NOT ${ARGV0} STREQUAL "NO_EXTRA_COMPILE_DEFINITIONS")
        target_compile_definitions(${NXA_PROJECT_NAME} PRIVATE "-DNXA_PROJECT_DIR=\"${CMAKE_CURRENT_SOURCE_DIR}\"")
    endif ()

    string(COMPARE EQUAL "${NXA_PROJECT_NAME}" "" _RESULT)
    if(_RESULT)
        message(FATAL_ERROR "Project is missing call to `nxa_project()` in its CMakeFile.")
    endif()

    if (TARGET ${NXA_PROJECT_NAME})
        get_target_property(target_type ${NXA_PROJECT_NAME} TYPE)
        if (NOT target_type STREQUAL "INTERFACE_LIBRARY")
            define_file_basename_for_sources(${NXA_PROJECT_NAME})
        endif()
    endif()

    # -- We need to figure out if we have a parent scope or not
    get_directory_property(hasParent PARENT_DIRECTORY)
    if(hasParent)
        if(NXA_CMAKE_BUILD_FOR_TESTING)
            if (NOT ${ARGC} OR NOT ${ARGV0} STREQUAL "DO_NOT_ADD_TO_ALL_TEST_SOURCE_FILES")
                # -- This is used to pass our list of source files to the parent project to that they can be added to
                # -- a single test executable currently needed by CLion in order to integrate the tests in the IDE
                set(NXA_ALL_TEST_SOURCE_FILES ${NXA_ALL_TEST_SOURCE_FILES} ${NXA_TEST_SOURCE_FILES})
            else()
                set(NXA_ALL_TEST_SOURCE_FILES ${NXA_ALL_TEST_SOURCE_FILES} PARENT_SCOPE)
            endif()

            list(LENGTH NXA_TEST_SOURCE_FILES numberOfTestSourceFiles)
            if (${numberOfTestSourceFiles})
                set(NXA_ALL_TEST_PROJECTS ${NXA_ALL_TEST_PROJECTS} ${NXA_PROJECT_NAME})
            endif()

            set(NXA_ALL_TEST_PROJECTS ${NXA_ALL_TEST_PROJECTS} PARENT_SCOPE)
            set(NXA_ALL_TEST_SOURCE_FILES ${NXA_ALL_TEST_SOURCE_FILES} PARENT_SCOPE)
        endif()

        # -- We propagate up all the libraries requiring binary injection to targets using them
        foreach(TARGET_NAME ${NXA_LIST_OF_LIBRARIES_REQUIRING_BINARY_INJECTION})
            set(NXA_LIBRARY_${TARGET_NAME}_REQUIRES_BINARY ${NXA_LIBRARY_${TARGET_NAME}_REQUIRES_BINARY} PARENT_SCOPE)
        endforeach()

        set(NXA_LIST_OF_LIBRARIES_REQUIRING_BINARY_INJECTION ${NXA_LIST_OF_LIBRARIES_REQUIRING_BINARY_INJECTION} PARENT_SCOPE)
    endif()

    set(NXA_PROJECT_NAME "")
    set(CMAKE_PROJECT_NAME "")
endmacro()

macro(library_requires_binary_injection target BINARY_INJECTION_PARAMETERS)
    string(COMPARE EQUAL "${NXA_PROJECT_NAME}" "" _RESULT)
    if(_RESULT)
        message(FATAL_ERROR "Project is missing call to `nxa_project()` in its CMakeFile.")
    endif()

    string(COMPARE EQUAL "${NXA_LIBRARY_${target}_REQUIRES_BINARY}" "" _RESULT)
    if(NOT _RESULT)
        message(FATAL_ERROR "Project ${target} is trying to inject two binaries. This is not currently supported.")
    endif()

    set(_BINARY_INJECTION_PARAMETERS_AS_LIST ${BINARY_INJECTION_PARAMETERS} ${ARGN})
    set(NXA_LIBRARY_${target}_REQUIRES_BINARY ${_BINARY_INJECTION_PARAMETERS_AS_LIST})

    message(STATUS "${target} library requires injection of ${_BINARY_INJECTION_PARAMETERS_AS_LIST}")

    list(APPEND NXA_LIST_OF_LIBRARIES_REQUIRING_BINARY_INJECTION ${target})
endmacro()

function(fix_qt_deps target qtdir)
    set(strip_or_not "-no-strip")

    add_custom_command(TARGET ${target}
                       POST_BUILD
                       COMMAND python ${NXA_TOP_PROJECT_DIR}/bin/macOS/qt_fix_deps.py "$<TARGET_BUNDLE_DIR:${target}>")
endfunction()

function(deploy_framework target)
    cmake_parse_arguments(ARG "" "FROM;NAME;VERSION" "" ${ARGN})

    set(target_binary $<TARGET_FILE:${target}>)
    set(dest_parent $<TARGET_FILE_DIR:${target}>)
    set(dest_parent ${dest_parent}/../Frameworks)
    set(dest ${dest_parent}/${ARG_NAME}.framework)
    set(full_input ${ARG_FROM}/${ARG_NAME}.framework)

    if(NOT ARG_VERSION)
        set(ARG_VERSION "A")
    endif()

    file(TO_NATIVE_PATH ${full_input} copy_source)

    add_custom_command(TARGET ${target}
                       POST_BUILD
                       COMMAND ${CMAKE_COMMAND} -E make_directory ${dest_parent}
                       COMMAND rm -Rf ${dest}
                       COMMAND cp -Rf ${full_input} ${dest_parent}
                       COMMAND find ${dest} -name *.prl -print0 | xargs -0 rm -f
                       COMMAND find ${dest} -name *_debug* -print0 | xargs -0 rm -f
                       COMMAND install_name_tool -change @rpath/${ARG_NAME}.framework/Versions/${ARG_VERSION}/${ARG_NAME} @executable_path/../Frameworks/${ARG_NAME}.framework/Versions/${ARG_VERSION}/${ARG_NAME} ${target_binary}
                       COMMAND install_name_tool -id @executable_path/../Frameworks/${ARG_NAME}.framework/Versions/${ARG_VERSION}/${ARG_NAME} ${dest_parent}/${ARG_NAME}.framework/Versions/${ARG_VERSION}/${ARG_NAME})
endfunction()

function(deploy_library target)
    cmake_parse_arguments(ARG "" "FROM;NAME" "" ${ARGN})

    set(dest_parent $<TARGET_FILE_DIR:${target}>)
    set(dest_parent "${dest_parent}/../MacOS")
    set(dest "${dest_parent}/lib${ARG_NAME}.dylib")
    set(full_input "${ARG_FROM}/lib/lib${ARG_NAME}.dylib")

    file(TO_NATIVE_PATH ${full_input} copy_source)

    add_custom_command(TARGET ${target}
                       POST_BUILD
                       COMMAND ${CMAKE_COMMAND} -E make_directory ${dest}
                       COMMAND ${CMAKE_COMMAND} -E copy ${copy_source} ${dest})
endfunction()

function(inject_binary target)
    cmake_parse_arguments(ARG "INSTALL;LIBRARY;FRAMEWORK;SEPARATESOURCEDEBUG;NOSEPARATEDESTDEBUG" "FROM;NAME;OUTSUBDIR" "" ${ARGN})

    if(ARG_OUTSUBDIR)
        set(dest $<TARGET_FILE_DIR:${target}>/${ARG_OUTSUBDIR})
    else()
        set(dest $<TARGET_FILE_DIR:${target}>)
    endif()

    set(command "copy")

    set(ARG_SOURCE_NAME "${ARG_NAME}")

    if(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        set(extension ".dll")

        if (ARG_SEPARATESOURCEDEBUG)
            if(CMAKE_BUILD_TYPE STREQUAL "Debug")
                set(ARG_SOURCE_NAME "${ARG_NAME}d")
                if(ARG_NOSEPARATEDESTDEBUG)
                    set(dest "${dest}/${ARG_NAME}${extension}")
                endif()
            endif()
        endif()
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        set(extension ".dylib")
        set(prefix "lib")

        if(ARG_FRAMEWORK)
            set(extension ".framework")
            set(prefix "")
            set(command "copy_directory")
            set(dest "${dest}/../${prefix}${ARG_NAME}${extension}")
        elseif(ARG_SEPARATESOURCEDEBUG)
            if(CMAKE_BUILD_TYPE STREQUAL "Debug")
                set(ARG_SOURCE_NAME "${ARG_NAME}_debug")
                if(ARG_NOSEPARATEDESTDEBUG)
                    set(dest "${dest}/${ARG_NAME}${extension}")
                endif()
            endif()
        endif()
    else()
        message(FATAL_ERROR "Unsupported platform.")
    endif()

    if(ARG_OUTSUBDIR OR ARG_FRAMEWORK)
        add_custom_command(TARGET ${target}
                           POST_BUILD
                           COMMAND ${CMAKE_COMMAND} -E make_directory ${dest})
    endif()

    set(full_input "${ARG_FROM}/${prefix}${ARG_SOURCE_NAME}${extension}")

    file(TO_NATIVE_PATH ${full_input} copy_source)

    add_custom_command(TARGET ${target}
                       POST_BUILD
                       COMMAND ${CMAKE_COMMAND} -E ${command} ${copy_source} ${dest})

    message(STATUS "Injecting ${full_input} for target ${target}.")

    if(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        if(ARG_INSTALL)
            if(ARG_OUTSUBDIR OR ARG_FRAMEWORK)
                set(full_output "./${ARG_OUTSUBDIR}")
            else()
                set(full_output ".")
            endif()

            install(FILES ${full_input} DESTINATION ${full_output})
        endif()
    endif()
endfunction()

function(inject_all_binaries_for TARGET)
    foreach(TARGET_NAME ${NXA_LIST_OF_LIBRARIES_REQUIRING_BINARY_INJECTION})
        inject_binary(${TARGET} ${NXA_LIBRARY_${TARGET_NAME}_REQUIRES_BINARY})
    endforeach()
endfunction()

# -- Set the default build type

macro(set_build_variables)
    set(MULTICONFIG OFF)

    # -- Worth reading https://stackoverflow.com/questions/24460486/cmake-build-type-not-being-used-in-cmakelists-txt
    if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
        set(default_build_type "Release")
        if(EXISTS "${CMAKE_SOURCE_DIR}/.git")
            set(default_build_type "Debug")
        endif()
        message(STATUS "Setting build type to '${default_build_type}' as none was specified.")
        set(CMAKE_BUILD_TYPE ${default_build_type} CACHE STRING "Choose the type of build" FORCE)
    elseif(NOT CMAKE_CONFIGURATION_TYPES)
        message(STATUS "Build type configured as '${CMAKE_BUILD_TYPE}'")
    else()
        message(STATUS "Multi-configuration project with types ${CMAKE_CONFIGURATION_TYPES}")
        set(MULTICONFIG ON)
    endif()
endmacro()

# -- setup OS_BUNDLE for platform

macro(set_os_bundle)
    if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        set(OS_BUNDLE MACOSX_BUNDLE)
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        set(OS_BUNDLE WIN32)
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Linux")
        set(OS_BUNDLE LINUX)
    else()
        message(FATAL_ERROR "Unsupported platform.")
    endif()
endmacro()

macro(set_win32_executable ARG_TARGET)
    if (MSVC)
        set_target_properties(${ARG_TARGET} PROPERTIES
                              WIN32_EXECUTABLE YES
                              LINK_FLAGS "/ENTRY:mainCRTStartup")
    endif()
endmacro()

macro(init_qt)
    set(CMAKE_AUTOUIC_SEARCH_PATHS ${CMAKE_CURRENT_SOURCE_DIR}/Objects)
    set(CMAKE_AUTOMOC ON) # For meta object compiler
    set(CMAKE_AUTORCC ON) # Resource files
    set(CMAKE_AUTOUIC ON) # UI files
endmacro()

# -- add metadata files to the project

macro(target_add_metadata ARG_TARGET)
    if(NOT RESOURCE_FOLDER)
        set(RESOURCE_FOLDER ${PROJECT_SOURCE_DIR})
    endif()

    if(NOT NXA_ICON_NAME)
        set(NXA_ICON_NAME AppIcon)
    endif()

    if(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        set(NXA_ICON_FILE ${RESOURCE_FOLDER}/${NXA_ICON_NAME}.ico)

        configure_file("${PROJECT_SOURCE_DIR}/windows_metafile.rc.in" "version.rc")

        set(RES_FILES "version.rc")
        set(CMAKE_RC_COMPILER_INIT windres)

        enable_language(RC)

        target_sources(${ARG_TARGET} PRIVATE ${RES_FILES})
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        set(NXA_ICON_FILE ${RESOURCE_FOLDER}/${NXA_ICON_NAME}.icns)

        set_source_files_properties(${NXA_ICON_FILE} PROPERTIES MACOSX_PACKAGE_LOCATION Resources)

        set(MACOSX_BUNDLE_COPYRIGHT ${COPYRIGHT})
        set(MACOSX_BUNDLE_GUI_IDENTIFIER ${NXA_CMAKE_IDENTIFIER})
        set(MACOSX_BUNDLE_ICON_FILE ${NXA_ICON_NAME})
        set(MACOSX_BUNDLE_MINIMUM_SYSTEM_VERSION "${CMAKE_OSX_DEPLOYMENT_TARGET}")

        target_sources(${ARG_TARGET} PRIVATE ${NXA_ICON_FILE})
    else()
        message(FATAL_ERROR "Unsupported platform.")
    endif()
endmacro()

detect_platform()
set_system_version()
set_build_variables()
set_os_bundle()

macro(nxa_depends_on_build_defines target project)
    add_dependencies(${target} ${project})
    target_include_directories(${target} PUBLIC ${CMAKE_BINARY_DIR})
endmacro()

if(RKB_CMAKE_VERBOSE)
    message(STATUS "CMake is set in VERBOSE mode.")
    set(CMAKE_VERBOSE_MAKEFILE on)
endif()

set(COMPANY "Didier Malenfant")
set(COPYRIGHT "Copyright (C) 2020-2021 ${COMPANY} (didier@rekordbuddy.org)")

message(STATUS "Platform is ${NXA_CMAKE_PLATFORM}.")

# -- Our options
option(NXA_CMAKE_BUILD_FOR_TESTING "Build/run testsuite and tweak functions that return machine state to instead use constant date/time and other machine traits" OFF)
option(NXA_CMAKE_ENABLE_EXTENDED_ERROR_DESCRIPTIONS "Print extended errors message." OFF)
option(NXA_CMAKE_DISABLE_APP_UPDATER "Disable checking for app updates" OFF)
option(NXA_CMAKE_TRADESHOW_DEMO_MODE "Skip save/load logic to make tradeshow demos easier" OFF)
option(NXA_CMAKE_DISABLE_LINK_TIME_OPTIMIZATIONS "Build binaries with link time optimizations, producing significant performance gains" OFF)
option(NXA_CMAKE_ENABLE_ADDRESS_SANITIZATION "On macOS, activate address sanitization (30-50% perf cut; detects memory bugs at runtime)" OFF)
option(NXA_CMAKE_PRINT_TRANSACTION_DURATION "Print the time each transaction takes, including whether it committed or rolled back" OFF)
option(NXA_CMAKE_BUILD_ALL_TESTS_APP "Build a single executable with all our tests for CLION." OFF)
option(NXA_CMAKE_RELEASE_BUILD_FOR_DEBUGGING "Add debugging info and settings to release builds." OFF)
option(NXA_CMAKE_BETA_BUILD "Builds a beta version of the app." OFF)

# -- When this is set, we don't allow any settings that would be unwanted in a production build.
if (NXA_CMAKE_THIS_IS_A_PRODUCTION_BUILD)
    set(NXA_STATUS_OR_FATAL FATAL_ERROR)
else()
    set(NXA_STATUS_OR_FATAL STATUS)
endif()

if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
    if(NXA_CMAKE_RELEASE_BUILD_FOR_DEBUGGING)
        add_definitions(-DNXA_RELEASE_BUILD_FOR_DEBUGGING)
        message(${NXA_STATUS_OR_FATAL} "‼️‼️‼️ This release build will skip some checks in order to be slightly more debuggable ‼️‼️‼️")
    endif()
endif()

message(STATUS "Version being built is ${RKB_CMAKE_VERSION_STRING}(${NXA_CMAKE_GIT_COMMIT_HASH}).")
message(STATUS "Debug build symbol version is ${RKB_CMAKE_SYMBOLS_VERSION_STRING}.")

# -- Compilation settings based on the platform
if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
    set(CMAKE_OSX_DEPLOYMENT_TARGET "10.13")
    set(ENV{MACOSX_DEPLOYMENT_TARGET} "${CMAKE_OSX_DEPLOYMENT_TARGET}")
    set(NXA_CMAKE_IDENTIFIER "org.rekordbuddy.RekordBuddy")
    message(STATUS "macOS deployment SDK: ${CMAKE_OSX_DEPLOYMENT_TARGET}")
endif()

# -- Standard compilation settings
set(CMAKE_CXX_STANDARD 17)
set(CLANG_DEFAULT_CXX_STDLIB "libc++")
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} -O3")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "${CMAKE_C_FLAGS_RELWITHDEBINFO} -O3")

# -- GLOBAL Settings based on the build type
# -- Note: Generator expressions must be used for build type, hence we must modify the property with set_property,
#          which is the only way to set the property with a generator expression
set_property(DIRECTORY ${CMAKE_SOURCE_DIR} APPEND PROPERTY COMPILE_DEFINITIONS $<$<CONFIG:Debug>:DEBUG=1>)

# -- GLOBAL Settings based on cmake options
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(NXA_CMAKE_BUILD_FOR_TESTING 1)
else()
    # -- Turn off debugging for things like sqlite
    add_definitions(-DNDEBUG=1)
endif()

if(NXA_CMAKE_BUILD_FOR_TESTING)
    message(${NXA_STATUS_OR_FATAL} "‼️‼️‼️ This build is a TESTING build ‼️‼️‼️")

    set(INSTALL_GTEST OFF)
    set(INSTALL_GMOCK OFF)

    add_definitions(-DNXA_BUILD_FOR_TESTING)
endif()

if (NXA_CMAKE_ENABLE_EXTENDED_ERROR_DESCRIPTIONS)
    message(${NXA_STATUS_OR_FATAL} "‼️‼️‼️ Extended error descriptions are ENABLED ‼️‼️‼️")
    add_definitions(-DNXA_ENABLE_EXTENDED_ERROR_DESCRIPTIONS)
else()
    add_definitions(-DOPENSSL_NO_ERR)
endif()

if(CMAKE_BUILD_TYPE STREQUAL "Debug" OR NXA_CMAKE_DISABLE_APP_UPDATER)
    message(${NXA_STATUS_OR_FATAL} "‼️‼️‼️ App updating is DISABLED ‼️‼️‼️")
    add_definitions(-DNXA_DISABLE_APP_UPDATER)
endif()

# -- Link time optimizations seem to prevent the symbol table from being output with filenames and lines in
# -- so we disable it for now.
set(NXA_CMAKE_DISABLE_LINK_TIME_OPTIMIZATIONS ON)

if(NXA_CMAKE_DISABLE_LINK_TIME_OPTIMIZATIONS)
    message(STATUS "Link time optimizations are DISABLED.")
else()
    if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
        # -- Note: Note quite working yet, needs a bit more work. Would produce nice benefits
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -flto")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -flto")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -flto")
    elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
        message(STATUS "Link time optimization not supported on Windows yet (CMake does not chose an appropriate linker)")
    else()
        message(FATAL_ERROR "Unsupported platform.")
    endif()
endif()

if(NXA_CMAKE_PRINT_TRANSACTION_DURATION)
    message(STATUS "Transaction timings are ENABLED.")
    add_definitions(-DNXA_PRINT_TRANSACTION_DURATION)
endif()

if(NXA_CMAKE_TRADESHOW_DEMO_MODE)
    message(${NXA_STATUS_OR_FATAL} "‼️‼️‼️ Trade show mode is ENABLED ‼️‼️‼️")
    add_definitions(-DNXA_TRADESHOW_DEMO_MODE)
endif()

if (NXA_CMAKE_ENABLE_ADDRESS_SANITIZATION)
    if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS" OR ${NXA_CMAKE_PLATFORM} STREQUAL "Linux")
        message(${NXA_STATUS_OR_FATAL} "‼️‼️‼️ Address sanitization is ENABLED ‼️‼️‼️")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer -fsanitize=address -fsanitize-address-use-after-scope")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-omit-frame-pointer -fsanitize=address -fsanitize-address-use-after-scope")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -fno-omit-frame-pointer -fsanitize=address -fsanitize-address-use-after-scope")
    else()
        message(FATAL_ERROR "Address sanitization is not supported on ${NXA_CMAKE_PLATFORM} yet.")
    endif()
else()
    message(STATUS "Address sanitization is DISABLED.")
endif()

# -- Compilation settings based on the platform
if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
    if(NOT CMAKE_BUILD_TYPE STREQUAL "Release")
        add_compile_options(-g)
    endif()

    add_compile_options(-ftemplate-backtrace-limit=0 -Wall -Werror -Wignored-qualifiers -Wmissing-field-initializers -Wmissing-method-return-type -Wold-style-cast -Wsemicolon-before-method-body -Wsign-compare)
    add_compile_options(-Wno-objc-property-no-attribute)
    add_compile_options("-mmacosx-version-min=${CMAKE_OSX_DEPLOYMENT_TARGET}")

    # -- For releases; we want symbols to be 'hidden' so they can be stripped. if you want it public, use EXPORT macro
    add_compile_options("-fvisibility-inlines-hidden")
    add_compile_options("-fvisibility=hidden")
elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    add_compile_options(-Wno-error)
    add_compile_options(-Wno-unused-command-line-argument)
    add_definitions(-DTAGLIB_STATIC=1)

    if(MSVC)
        add_compile_options(/source-charset:utf-8)
        add_compile_options(/EHa)

        # -- This makes sure that all member function methods pointers use the most general format possible.
        # -- Otherwise, the MS compiler can try to optimize/guess the size on classes that are not yet fully known
        # -- and if it guesses wrongly in one source file, but correctly in another, the program will inexplicably crash at runtime.
        # -- More info here: https://www.codeproject.com/Articles/7150/Member-Function-Pointers-and-the-Fastest-Possible
        add_compile_options(/vmg)

        if(CMAKE_BUILD_TYPE STREQUAL "Debug")
            add_compile_options("/MDd")
        else()
            add_compile_options("/MD")
        endif()

        # -- TODO: Port rest of warnings from unix-style args
        if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
            # -- TODO: We should add `-Xclang -ftemplate-backtrace-limit=0` here but the compiler doesn't accept it?
            add_compile_options(-Wno-c++17-extensions)
        endif()
    endif()

    add_definitions(-DNXA_SKIP_COCOA_PROFILING_SIGNPOSTS)

    # -- The following definitions are temporary until we refactor the deprecated uses they are pointing out.
    add_definitions(-D_SILENCE_CXX17_CODECVT_HEADER_DEPRECATION_WARNING -D_SILENCE_CXX17_SHARED_PTR_UNIQUE_DEPRECATION_WARNING -D_SILENCE_CXX17_RESULT_OF_DEPRECATION_WARNING)
elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Linux")
    if(NOT CMAKE_BUILD_TYPE STREQUAL "Release")
        add_compile_options(-g)
    endif()

    add_compile_options(-Wall -Werror -Wignored-qualifiers -Wmissing-field-initializers -Wold-style-cast -Wsign-compare)
    add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-ftemplate-backtrace-limit=0>)

    # -- For releases; we want symbols to be 'hidden' so they can be stripped. if you want it public, use EXPORT macro
    add_compile_options($<$<COMPILE_LANGUAGE:CXX>:-fvisibility-inlines-hidden>)
    add_compile_options(-fvisibility=hidden)
else()
    message(FATAL_ERROR "Unsupported platform.")
endif()

# -- This has to be inserted before ANY add_subdirectory commands
if(NXA_CMAKE_BUILD_FOR_TESTING)
    add_subdirectory(Vendor/googletest)

    target_uses_dynamic_runtime(gmock)
    target_uses_dynamic_runtime(gmock_main)
    target_uses_dynamic_runtime(gtest)

    set(NXA_TESTING_FRAMEWORK_LIBS gtest gmock_main gmock)
endif()

# -- Set up our CMake subprojects
add_subdirectory(Base)

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS" OR ${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    add_subdirectory(Vendor/Sparkle)
    add_subdirectory(Vendor/taglib)
    add_subdirectory(Vendor/SQLiteCpp)

    add_subdirectory(Persistence)
endif()

add_subdirectory(TrackFiles)
add_subdirectory(Collections)

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS" OR ${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    # -- This contains the main Rekord Buddy project which uses info from the other projects so it's good to keep it last.
    add_subdirectory(UI)
endif()

# -- This is inserted last so that we gather up any tests source files into our macro test project (used for CLion)
if(NXA_CMAKE_BUILD_FOR_TESTING)
    add_subdirectory(Tests)
endif()
