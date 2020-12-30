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

#include <Base/UserPreferences.hpp>
#include <Base/FilePath.hpp>
#include <Base/Test.hpp>

namespace NxA {

// -- Public Interface
class UserPreferencesTests : public NxA::Test
{
protected:
    // -- Protected Contants
    static inline constexpr byte p_blobTestData[] = { 0x01, 0x02, 0x00, 0x03, 0x04, 0xff, 0x17 };

    // -- Protected Class Methods
    boolean p_loadDefaultsFromFile(UserPreferences& userPreferences, const FilePath& path)
    {
        return userPreferences.p_loadFromFile(path);
    }
    boolean p_saveDefaultsToFile(const UserPreferences& userPreferences, const FilePath& path) const
    {
        return userPreferences.saveToFile(path);
    }

public:
};

}
