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

// -- This file contains windows specific implementations.
static_assert(NXA_PLATFORM_WINDOWS, "Invalid platform.");

#include <Base/UserPreferences.hpp>

using namespace NxA;

// -- Interface

Optional<boolean> UserPreferences::maybeOSBooleanForKey(const String&)
{
    // -- OS USER DEFAULTS NOT IMPLEMENTED ON THIS PLATFORM
    return nothing;
}

Optional<integer32> UserPreferences::maybeOSIntegerForKey(const String&)
{
    // -- OS USER DEFAULTS NOT IMPLEMENTED ON THIS PLATFORM
    return nothing;
}

Optional<String> UserPreferences::maybeOSStringForKey(const String&)
{
    // -- OS USER DEFAULTS NOT IMPLEMENTED ON THIS PLATFORM
    return nothing;
}
