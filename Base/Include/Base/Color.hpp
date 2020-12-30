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
#include <Base/Platform.hpp>

namespace NxA {

// -- Public Interface
class Color final
{
    // -- Private Instance Variables
    union {
        uinteger32 asInteger;
        byte asBytes[4];
    } components;

public:
    // -- Constructors & Destructors
    Color(byte red, byte green, byte blue, byte alpha = 0xff)
    {
        if (Platform::endianOrder() == Platform::Endian::Little) {
            this->components.asBytes[0] = alpha;
            this->components.asBytes[1] = blue;
            this->components.asBytes[2] = green;
            this->components.asBytes[3] = red;
        }
        else if (Platform::endianOrder() == Platform::Endian::Big) {
            this->components.asBytes[0] = red;
            this->components.asBytes[1] = green;
            this->components.asBytes[2] = blue;
            this->components.asBytes[3] = alpha;
        }
        else {
            NXA_ALOG("Unsupported Platform.");
        }
    }
    constexpr Color(uinteger32 rgba) : components{ rgba } { }

    // -- Operators
    inline bool operator==(const Color& other) const
    {
        return (this->components.asInteger == other.components.asInteger);
    }
    inline bool operator!=(const Color& other) const
    {
        return (this->components.asInteger != other.components.asInteger);
    }
    inline bool operator<(const Color& other) const
    {
        return (this->components.asInteger < other.components.asInteger);
    }

    // -- Instance Methods
    inline byte red() const
    {
        if (Platform::endianOrder() == Platform::Endian::Little) {
            return this->components.asBytes[3];
        }
        else if (Platform::endianOrder() == Platform::Endian::Big) {
            return this->components.asBytes[0];
        }
        else {
            NXA_ALOG("Unsupported Platform.");
        }
    }
    inline byte green() const
    {
        if (Platform::endianOrder() == Platform::Endian::Little) {
            return this->components.asBytes[2];
        }
        else if (Platform::endianOrder() == Platform::Endian::Big) {
            return this->components.asBytes[1];
        }
        else {
            NXA_ALOG("Unsupported Platform.");
        }
    }
    inline byte blue() const
    {
        if (Platform::endianOrder() == Platform::Endian::Little) {
            return this->components.asBytes[1];
        }
        else if (Platform::endianOrder() == Platform::Endian::Big) {
            return this->components.asBytes[2];
        }
        else {
            NXA_ALOG("Unsupported Platform.");
        }
    }
    inline byte alpha() const
    {
        if (Platform::endianOrder() == Platform::Endian::Little) {
            return this->components.asBytes[0];
        }
        else if (Platform::endianOrder() == Platform::Endian::Big) {
            return this->components.asBytes[3];
        }
        else {
            NXA_ALOG("Unsupported Platform.");
        }
    }

    inline uinteger32 asRGB() const
    {
        return (this->components.asInteger >> 8) & 0xffffff;
    }
    inline uinteger32 asRGBA() const
    {
        return this->components.asInteger;
    }

    inline size_t hash() const
    {
        return this->asRGBA();
    }
};

}
