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
#include <CommonCollection/Markers/Marker.hpp>

#include <Base/Color.hpp>
#include <Base/Optional.hpp>
#include <Base/Flags.hpp>

namespace NxA { namespace Common {

// -- Private Interface (which can be used by unit tests).
#if defined(NXA_BUILD_FOR_TESTING)
extern Array<Color> p_seratoDefaultMarkerColors;
extern Color p_rekordboxDefaultMarkerColor;
extern Color p_rekordboxDefaultLoopMarkerColor;

extern Map<Color, Color> p_rekordboxMarkerColorFor;
extern Map<Color, Color> p_seratoMarkerColorFor;
#endif

// -- Public Interface
class MarkerColor
{
public:
    // -- Constants
    enum class IsALoopMarker
    {
        No,
        Yes
    };

    constexpr static inline Color YellowColor { 0xcccc00ffu };
    constexpr static inline Color OrangeColor { 0xff8c00ffu };
    constexpr static inline Color TurquoiseColor { 0x00ccccffu };
    constexpr static inline Color GreenColor { 0x00cc00ffu };

    // -- Class Methods
    static Optional<Color> maybeColorForMarkerColorAndMaybeHotCueNumberWhenExportedTo(const Color&,
                                                                                      Optional<count>,
                                                                                      Common::Collection::Type,
                                                                                      IsALoopMarker = IsALoopMarker::No,
                                                                                      NxA::Flags<NxA::Common::Marker::Flag> = NxA::Flags<NxA::Common::Marker::Flag>{ });
};

} }
