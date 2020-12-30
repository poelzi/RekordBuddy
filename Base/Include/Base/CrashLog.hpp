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

#pragma once

#include <Base/Types.hpp>

namespace NxA {

// -- Forward Declarations
class AssertionFailed;
class String;

// -- Public Interface
class CrashLog
{
public:
    // -- Class Methods
    static void initWithAPIKeyAndAppVersion(const String&, const String&);
    static void setUserID(const String&);

#if defined(NXA_PLATFORM_WINDOWS)
    // -- On Windows, we don't have access to a real stacktrace and some other info for now so we make do with this.
    static void setFileMethodNamesAndLineNumber(const char*, const char*, count);
    static void setAppBundleID(const String&);
#endif

    static boolean hasCrashedInLastSession();

    static const AssertionFailed& haltWith(const AssertionFailed&);
    static void notify(const String&);

    static void addUserInfoWithKey(const String&, const String&);
    static void addBreadCrumb(const String&);
};

}
