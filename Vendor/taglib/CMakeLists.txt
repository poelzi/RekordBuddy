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

if("${NXA_TAGLIB_DIR}" STREQUAL "")
    message(FATAL_ERROR "NXA_TAGLIB_DIR is not defined. Must point to your taglib build folder install.")
endif()

nxa_project(TagLib)

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
    add_library(TagLib INTERFACE)

    find_library(VendoredTagLib tag "${NXA_TAGLIB_DIR}" NO_DEFAULT_PATH)
    mark_as_advanced(VendoredTagLib)

    if (NOT VendoredTagLib)
        message(FATAL_ERROR "TagLib required, but not found!")
    endif()

    target_link_libraries(TagLib INTERFACE z ${VendoredTagLib})
elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
    add_library(TagLib STATIC IMPORTED GLOBAL)

    library_requires_binary_injection(TagLib LIBRARY INSTALL SEPARATESOURCEDEBUG NOSEPARATEDESTDEBUG FROM "${NXA_TAGLIB_DIR}/bin" NAME "tag")

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(TagLibFileName "tagd")
    else()
        set(TagLibFileName "tag")
    endif()

    find_library(VendoredTagLib
                 NAMES ${TagLibFileName}
                 HINTS "${NXA_TAGLIB_DIR}/lib"
                 NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_PATH NO_CMAKE_ENVIRONMENT_PATH NO_DEFAULT_PATH)
    if (NOT VendoredTagLib)
        message(FATAL_ERROR "TagLib required, but not found!")
    endif()

    set_target_properties(TagLib PROPERTIES
                          IMPORTED_LOCATION ${VendoredTagLib}
                          INTERFACE_INCLUDE_DIRECTORIES "C:/Users/didier/Documents/Code/RekordBuddy/Vendor/taglib/Windows/include")
else()
    message(FATAL_ERROR "Unsupported platform.")
endif()

if (NOT VendoredTagLib)
    message(FATAL_ERROR "TagLib required, but not found!")
endif()

message(STATUS "TagLib found at ${VendoredTagLib}.")

nxa_project_end("NO_EXTRA_COMPILE_DEFINITIONS")
