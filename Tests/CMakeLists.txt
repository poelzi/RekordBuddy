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

enable_testing()

add_custom_target(RekordBuddyBuildAllTestsTarget)
add_dependencies(RekordBuddyBuildAllTestsTarget ${NXA_ALL_TEST_PROJECTS})

message(STATUS "All Test Projects: ${NXA_ALL_TEST_PROJECTS}")

if (NXA_CMAKE_BUILD_ALL_TESTS_APP)
    message(STATUS "All Test Sources: ${NXA_ALL_TEST_SOURCE_FILES}")

    list(LENGTH NXA_ALL_TEST_SOURCE_FILES numberOfTestSources)
    if (${numberOfTestSources})
        # -- This project is required in order to integrate the google tests into the CLion IDE. CLion does not support ctest yet but supports
        # -- a single executable built with the google test framework. This project below builds that executable using the same sources that
        # -- are used in out unit test projects. This ensures that the list of tests will match between this project and ctest.
        nxa_project(RekordBuddyAllTestsApp)

        add_executable(RekordBuddyAllTestsApp ${NXA_ALL_TEST_SOURCE_FILES})

        add_dependencies(RekordBuddyAllTestsApp gtest)
        target_compile_definitions(RekordBuddyAllTestsApp PRIVATE "-DNXA_TRACKFILES_DIR=\"${NXA_TRACKFILES_DIR}\"")
        target_link_libraries(RekordBuddyAllTestsApp PRIVATE Base Persistence Collections)

        # -- This project does not include the other test projects as libraries so we need to add some include paths manually for some of those.
        target_include_directories(RekordBuddyAllTestsApp
                                   PRIVATE ../Collections/RekordBuddy/Tests/Implementation/Include
                                           ../Collections/Serato/Objects/Implementation/Objects)

        inject_all_binaries_for(RekordBuddyAllTestsApp)

        add_test(NAME RekordBuddyAllTestsApp COMMAND RekordBuddyAllTestsApp)

        nxa_project_end("DO_NOT_ADD_TO_ALL_TEST_SOURCE_FILES")
    endif()
endif()
