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

#include <Base/Blob.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class CratePath
{
    // -- Private Instance Methods
    MutableBlob p_data;

public:
    // -- Factory Methods
    static CratePath forCrateNamed(const String&);

    // -- Operators
    bool operator==(const CratePath& other) const noexcept
    {
        return this->p_data == other.p_data;
    }
    bool operator<(const CratePath& other) const noexcept
    {
        return this->p_data < other.p_data;
    }

    // -- Instance Methods
    String crateName() const;

    CratePath pathForChildNamed(const String&) const;
    CratePath pathWithParentNamed(const String&) const;

    Optional<CratePath> maybeParentCratePath() const;

    String asString() const;
};

} }
