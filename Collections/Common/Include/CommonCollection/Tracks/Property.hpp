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

#include <Base/Base.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class Property
{
public:
    // -- Constants

    // -- These are stored in user data and should not be modified.
    // -- New IDS can be added to the list though.
    // -- Care must be taken to not overlap types with items. e.g., (id & ValueTypeMask) == 0
    using TypeIDStorage = byte;

    constexpr static TypeIDStorage OtherValueType       = 0b00000000;
    constexpr static TypeIDStorage TimeValueType        = 0b10000000;
    constexpr static TypeIDStorage StringValueType      = 0b01000000;
    constexpr static TypeIDStorage IntegerValueType     = 0b11000000;
    constexpr static TypeIDStorage DecimalValueType     = 0b00100000;
    constexpr static TypeIDStorage ValueTypeMask        = TimeValueType | IntegerValueType | StringValueType | DecimalValueType | OtherValueType;

    enum class TypeID : TypeIDStorage {
        // -- String Properties.
        Album                       = StringValueType | 0x00,
        Comments                    = StringValueType | 0x01,
        Genre                       = StringValueType | 0x02,
        Grouping                    = StringValueType | 0x03,
        MixName                     = StringValueType | 0x04,
        RecordLabel                 = StringValueType | 0x05,
        Tag                         = StringValueType | 0x06,
        Title                       = StringValueType | 0x07,
        DateAdded                   = StringValueType | 0x08,
        DateReleased                = StringValueType | 0x09,

        // -- Integer Properties.
        BeatGridLockedFlag          = IntegerValueType | 0x00,
        BitDepthInBits              = IntegerValueType | 0x01,
        BitRateInKiloBitsPerSecond  = IntegerValueType | 0x02,
        Color                       = IntegerValueType | 0x03,
        FileSize                    = IntegerValueType | 0x04,
        MusicalKey                  = IntegerValueType | 0x05,
        TrackNumber                 = IntegerValueType | 0x06,
        PlayCount                   = IntegerValueType | 0x07,
        Rating                      = IntegerValueType | 0x08,
        SampleRateInHertz           = IntegerValueType | 0x09,
        FileType                    = IntegerValueType | 0x0a,
        DiscNumber                  = IntegerValueType | 0x0b,

        // -- Decimal Properties.
        BeatsPerMinute              = DecimalValueType | 0x00,
        LengthInSeconds             = DecimalValueType | 0x01,

        // -- Time Properties.
        LastModifiedOn              = TimeValueType | 0x00,

        // -- Other Properties not coming from tags.
        RelativeFilePath            = OtherValueType | 0x00,
        NumberOfTracks              = OtherValueType | 0x01,
        ArtistCredit                = OtherValueType | 0x02,
        ProducerCredit              = OtherValueType | 0x03,
        RemixerCredit               = OtherValueType | 0x04,

        // -- This is required for static assertions.
        LastFlag
    };

    // -- We store these as a character so we need to make sure the casts are safe.
    static_assert(sizeof(TypeID) == sizeof(character), "Property::TypeID should be the size of a character.");

    // -- Types
    using PropertyTypeFlags = Flags<TypeID>;

    // -- Constructors & Destructors
    Property() = delete;

    // -- Class Methods
    inline static TypeIDStorage valueTypeIDFromPropertyType(Property::TypeID type)
    {
        return static_cast<TypeIDStorage>(type) & Property::ValueTypeMask;
    }
    static String nameForType(Property::TypeID);
};

} }
