//
//  Rekord Buddy - The future proof music collection tool made by DJs for DJs.
//  Copyright (C) 2020-2021 Didier Malenfant (didier@rekordbuddy.org)
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.
//

// -- This file contains linux specific implementations.
static_assert(NXA_PLATFORM_LINUX, "Invalid platform.");

#include <Base/Array.hpp>
#include <Base/Platform.hpp>
#include <Base/String.hpp>

#include <sys/types.h>
#include <sys/sysctl.h>

using namespace NxA;

// -- Class Methods

Optional<String> Platform::maybeUniqueIDForComputer()
{
    static Optional<String> maybeUniqueIDCache;
    if (!maybeUniqueIDCache.isValid()) {
        NXA_ALOG("To be implemented.");

        MutableString resultAsString{ "l" };

        maybeUniqueIDCache = resultAsString;
    }

    return *maybeUniqueIDCache;
}

Optional<String>& Platform::maybeUsername()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Platform::p_testUserName.isValid()) {
        static Optional<String> maybeUsername{ *Platform::p_testUserName.get() };
        return maybeUsername;
    }
#endif

    NXA_ALOG("To be implemented.");
}

String Platform::osVersion()
{
    NXA_ALOG("To be implemented.");
}

String Platform::hardwareName()
{
    NXA_ALOG("To be implemented.");
}

boolean Platform::isRunningFromInstallationFolder()
{
    // -- To be implemented.
    return false;
}

Array<String> Platform::namesOfOtherRunningApplications()
{
    MutableArray<String> results;

    // -- To be implemented.

    return { std::move(results) };
}

void Platform::alertWithTextAndInformativeText(const String& text, const String& informativeText)
{
    NXA_ALOG("To be implemented.");
}
