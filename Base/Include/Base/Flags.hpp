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
#include <Base/String.hpp>

namespace NxA {

template <typename FlagsEnum>
    class Flags
    {
        // -- Instance Variables
        uinteger64 flags{ 0 };

        static_assert(static_cast<integer>(FlagsEnum::LastFlag) < 64,
                      "E must be an enum class smaller than 64 values with a LastFlag entry larger than any value in the enum");

    public:
        // -- Constructors/Destructors
        Flags() = default;
        Flags(FlagsEnum value) : flags(1ULL << static_cast<integer>(value)) { }
        explicit Flags(uinteger64 packedValue) : flags(packedValue) { }

        // -- Operators
        operator bool() const
        {
            return this->flags != 0;
        }

        // -- Instance Methods
        bool has(FlagsEnum value) const
        {
            return this->flags & (1ULL << static_cast<integer>(value));
        }
        bool isEmpty() const
        {
            return this->flags == 0;
        }

        Flags<FlagsEnum> andAlso(FlagsEnum value) const
        {
            return Flags{ this->flags | (1ULL << static_cast<integer>(value)) };
        }
        Flags<FlagsEnum> andAlso(const Flags& other) const
        {
            return Flags{ this->flags | other.flags };
        }
        Flags<FlagsEnum> without(FlagsEnum value) const
        {
            return Flags{ this->flags & ~(1ULL << static_cast<integer>(value)) };
        }
        void set(FlagsEnum value)
        {
            this->flags |= (1ULL << static_cast<integer>(value));
        }
        void clear(FlagsEnum value)
        {
            this->flags &= ~(1ULL << static_cast<integer>(value));
        }
        void clearAll()
        {
            this->flags = 0;
        }

        inline uinteger64 asPackedValue() const noexcept    // -- Platform independent, can be used for archiving/unarchiving.
        {
            return this->flags;
        }
    };
    
}
