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

#include <Base/Optional.hpp>
#include <Base/String.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class TrackRating
{
    count p_value;

    // -- Private Constructors & Destructors
    TrackRating(count value) : p_value(value) { }

public:
    // -- Constants
    enum class Stars
    {
        Zero,
        One,
        Two,
        Three,
        Four,
        Five,
    };

    // -- Constructors & Destructors
    TrackRating() : p_value(0) { }
    TrackRating(Stars fromStarCount) : p_value(static_cast<count>(fromStarCount) * 51)
    {
        // -- Produces a rating suitable for writing to ID3v2 POPM tags, or exporting in most files.
        // -- Unknown -> 0, 1 star -> 51, 2 stars -> 102, 3 stars -> 153, 4 stars -> 204, 5 stars -> 255
    }

    // -- Factory Methods
    static Optional<TrackRating> maybeWithValue(integer value)
    {
        if ((value < 0) || (value > 255)) {
            return nothing;
        }

        return TrackRating{ static_cast<count>(value) };
    }

    // -- Operators
    inline bool operator==(const TrackRating& other) const noexcept
    {
        return this->p_value == other.p_value;
    }
    inline bool operator!=(const TrackRating& other) const noexcept
    {
        return this->p_value != other.p_value;
    }
    inline bool operator<(const TrackRating& other) const noexcept
    {
        return this->p_value < other.p_value;
    }

    // -- Instance Methods
    count value() const
    {
        return this->p_value;
    }
    Stars asStarCount() const;
    String asString() const;
};

} }
