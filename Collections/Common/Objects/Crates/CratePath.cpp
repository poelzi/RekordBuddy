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

#include <CommonCollection/Crates/CratePath.hpp>

#include <Base/String.hpp>

using namespace NxA;
using namespace NxA::Common;

// -- Factory Methods

CratePath CratePath::forCrateNamed(const String& name)
{
    NXA_ASSERT_FALSE(name.isEmpty());

    auto newPath = CratePath();
    newPath.p_data.appendWithStringTermination(name.asUTF8());

    NXA_ASSERT_TRUE(newPath.p_data.size() > 1);

    return newPath;
}

// -- Instance Methods

CratePath CratePath::pathForChildNamed(const String& childName) const
{
    NXA_ASSERT_FALSE(childName.isEmpty());

    auto newPath = CratePath();
    newPath.p_data.append(this->p_data);
    newPath.p_data.appendWithStringTermination(childName.asUTF8());

    NXA_ASSERT_TRUE(newPath.p_data.size() > 1);

    return newPath;
}

CratePath CratePath::pathWithParentNamed(const String& parentName) const
{
    NXA_ASSERT_FALSE(parentName.isEmpty());

    auto newPath = CratePath();
    newPath.p_data.appendWithStringTermination(parentName.asUTF8());
    newPath.p_data.append(this->p_data);

    NXA_ASSERT_TRUE(newPath.p_data.size() > 1);

    return newPath;
}

Optional<CratePath> CratePath::maybeParentCratePath() const
{
    count length = this->p_data.size();
    if (length < 4) {
        // -- This path cannot have a parent if it is so small.
        return nothing;
    }

    count index = length - 2;
    auto bytes = this->p_data.data();
    while ((index > 0) && (bytes[index] != 0u)) {
        --index;
    }

    if (index == 0u) {
        return nothing;
    }

    auto newPath = CratePath();
    newPath.p_data = MutableBlob::withMemoryAndSize(bytes, index + 1);

    NXA_ASSERT_TRUE(newPath.p_data.size() > 1);

    return { newPath };
}

String CratePath::crateName() const
{
    auto startIndex = this->p_data.size() - 2;
    auto bytes = this->p_data.data();

    while (startIndex && (bytes[startIndex] != 0)) {
        --startIndex;
    }

    if (bytes[startIndex] == 0) {
        ++startIndex;
    }

    return String::stringWithUTF8(reinterpret_cast<const character*>(&bytes[startIndex]));
}

String CratePath::asString() const
{
    MutableString result;

    count length = this->p_data.size();
    auto bytes = this->p_data.data();

    for (count index = 0; index < length; ++index) {
        char character = bytes[index];
        if (character != 0) {
            result.appendStringWithFormat("%c", character);
        }
        else if (index != (length - 1)) {
            result.append("->");
        }
    }

    return std::move(result);
}
