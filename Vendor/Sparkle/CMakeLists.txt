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

nxa_project(Sparkle)

add_library(Sparkle INTERFACE)

if(${NXA_CMAKE_PLATFORM} STREQUAL "macOS")
	find_library(SparkleFramework Sparkle "macOS" NO_DEFAULT_PATH)
	mark_as_advanced(SparkleFramework)

	if (NOT SparkleFramework)
		message(FATAL_ERROR "Sparkle framework required, but not found!")
	endif()

	target_link_libraries(Sparkle INTERFACE ${SparkleFramework})

	message(STATUS "Sparkle framework found at ${SparkleFramework}.")
elseif(${NXA_CMAKE_PLATFORM} STREQUAL "Windows")
	find_library(VendoredWinSparkle
				 NAMES WinSparkle
				 HINTS "Windows/x64/Release"
				 NO_SYSTEM_ENVIRONMENT_PATH NO_CMAKE_SYSTEM_PATH NO_CMAKE_PATH NO_CMAKE_ENVIRONMENT_PATH NO_DEFAULT_PATH)

	if (NOT VendoredWinSparkle)
		message(FATAL_ERROR "WinSparkle stub required, but not found! (see Windows/x64/Release)")
	endif()

	target_include_directories(Sparkle INTERFACE Windows/include)
	target_link_libraries(Sparkle INTERFACE ${VendoredWinSparkle})

	library_requires_binary_injection(Sparkle LIBRARY INSTALL FROM "${CMAKE_CURRENT_SOURCE_DIR}/Windows/x64/Release" NAME "WinSparkle")

	message(STATUS "Sparkle found at ${VendoredWinSparkle}.")
else()
	message(FATAL_ERROR "Unsupported platform.")
endif()

nxa_project_end("NO_EXTRA_COMPILE_DEFINITIONS")
