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

#include <CommonCollection/Tracks/Property.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class TrackDisplayCache
{
public:
    // -- Static Methods
    static void addStringValueForTagOfTypeToBlob(const String&, Common::Property::TypeID, MutableBlob&);
    static Array<String> stringValuesForTagOfTypeInBlob(Common::Property::TypeID, const MutableBlob&);

    static void addIntegerValueForTagOfTypeToBlob(integer32, Common::Property::TypeID, MutableBlob&);
    static Array<integer32> integerValuesForTagOfTypeInBlob(Common::Property::TypeID, const MutableBlob&);

    static void addDecimalValueForTagOfTypeToBlob(const DecimalNumber&, Common::Property::TypeID, MutableBlob&);
    static Array<DecimalNumber> decimalValuesForTagOfTypeInBlob(Common::Property::TypeID, const MutableBlob&);

    static void addTimeValueForTagOfTypeToBlob(const Time&, Common::Property::TypeID, MutableBlob&);
    static Array<Time> timeValuesForTagOfTypeInBlob(Common::Property::TypeID, const MutableBlob&);
};

} }
