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

#include <CommonCollection/Tracks/TrackRating.hpp>

using namespace NxA;
using namespace Common;

// -- Instance Methods

TrackRating::Stars TrackRating::asStarCount() const
{
    // -- To be compatible with Serato and Rekordbox, this code implements:
    // -- Unrated -> 0, 1-51 -> 1, 52-102 -> 2, 103-153 -> 3, 154-204 -> 4, 205-anything -> 5
    auto value = this->p_value;

    if (value == 0) {
        return Stars::Zero;
    }

    if (value < 52) {
        return Stars::One;
    }

    if (value < 103) {
        return Stars::Two;
    }

    if (value < 154) {
        return Stars::Three;
    }

    if (value < 205) {
        return Stars::Four;
    }

    return Stars::Five;
}

String TrackRating::asString() const
{
    switch (this->asStarCount()) {
        case Stars::Zero: {
            return { };
        }
        case Stars::One: {
            return String("⭐");
        }
        case Stars::Two: {
            return String("⭐⭐");
        }
        case Stars::Three: {
            return String("⭐⭐⭐");
        }
        case Stars::Four: {
            return String("⭐⭐⭐⭐");
        }
        case Stars::Five: {
            return String("⭐⭐⭐⭐⭐");
        }
    }
}
