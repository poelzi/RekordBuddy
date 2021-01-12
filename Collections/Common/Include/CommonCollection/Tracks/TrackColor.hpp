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

#include <CommonCollection/Collection.hpp>

#include <Base/Color.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class TrackColor
{
public:
    // -- Constants
    constexpr static inline Color RedColor{ 0xFF0000ffu };
    constexpr static inline Color OrangeColor{ 0xfa8d29ffu };
    constexpr static inline Color YellowColor{ 0xffff00ffu };
    constexpr static inline Color GreenColor{ 0x40ff40ffu };
    constexpr static inline Color TurquoiseColor{ 0x25FDE9ffu };
    constexpr static inline Color MagentaColor{ 0xff40ebffu };
    constexpr static inline Color BlueColor{ 0x0000FFffu };
    constexpr static inline Color PurpleColor{ 0x660099ffu };

    // -- Class Methods
    static Optional<Color> maybeColorForTrackColorWhenExportedTo(const Color&, Common::Collection::Type);
};

} }
