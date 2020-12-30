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

// -- This file contains macOS specific implementations.
static_assert(NXA_PLATFORM_MACOS, "Invalid platform.");

#include <Base/UserPreferences.hpp>

#import <Foundation/Foundation.h>

using namespace NxA;

// -- Static Variables

static NSUserDefaults *p_userDefaults = nil;

// -- Class Methods

Optional<boolean> UserPreferences::maybeOSBooleanForKey(const String& key)
{
    if (p_userDefaults == nil) {
        p_userDefaults = [NSUserDefaults standardUserDefaults];
    }

    if ([p_userDefaults objectForKey:[NSString stringWithUTF8String:key.asUTF8()]] == nil) {
        return nothing;
    }

    return [p_userDefaults boolForKey:[NSString stringWithUTF8String:key.asUTF8()]];
}

Optional<integer32> UserPreferences::maybeOSIntegerForKey(const String& key)
{
    if (p_userDefaults == nil) {
        p_userDefaults = [NSUserDefaults standardUserDefaults];
    }

    if ([p_userDefaults objectForKey:[NSString stringWithUTF8String:key.asUTF8()]] == nil) {
        return nothing;
    }

    return [p_userDefaults integerForKey:[NSString stringWithUTF8String:key.asUTF8()]];
}

Optional<String> UserPreferences::maybeOSStringForKey(const String& key)
{
    if (p_userDefaults == nil) {
        p_userDefaults = [NSUserDefaults standardUserDefaults];
    }

    if ([p_userDefaults objectForKey:[NSString stringWithUTF8String:key.asUTF8()]] == nil) {
        return nothing;
    }

    return String{ [[p_userDefaults stringForKey:[NSString stringWithUTF8String:key.asUTF8()]] UTF8String] };
}
