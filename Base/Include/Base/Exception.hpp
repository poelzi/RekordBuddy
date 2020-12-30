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

#include <cstdio>
#include <cstdarg>
#include <stdexcept>

namespace NxA {

// -- Public Interface

class Exception final : public std::runtime_error
{
public:
    // -- Class Methods
    static Exception with(const character* format, ...)
    {
        constexpr size_t formatStringBufferSize = 256;
        character buffer[formatStringBufferSize];

        va_list args;
        va_start(args, format);

        vsnprintf(buffer, formatStringBufferSize, format, args);

        va_end(args);

        return Exception{ buffer };
    }

    // -- Constructors & Destructors
    explicit Exception(const character* reason) : std::runtime_error(reason) { }
    virtual ~Exception() = default;
};

}
