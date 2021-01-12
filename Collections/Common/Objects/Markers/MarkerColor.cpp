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

#include <CommonCollection/Markers/MarkerColor.hpp>

#include <Base/Array.hpp>
#include <Base/Map.hpp>

namespace NxA { namespace Common {

// -- Static Variables

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Array<Color> p_seratoDefaultMarkerColors {
        // -- These are the standard colors used by Serato when creating new hotcues
        0xcc0000ffu,
        0xcc8800ffu,
        0x0000ccffu,
        MarkerColor::YellowColor,
        MarkerColor::GreenColor,
        0xcc00ccffu,
        MarkerColor::TurquoiseColor,
        0x8800ccffu,
};

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Color p_rekordboxDefaultMarkerColor {
        // -- This is rekordbox's standard green cue marker color
        0x28e214ffu,
};

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Color p_rekordboxDefaultLoopMarkerColor {
        // -- This is rekordbox's standard orange loop marker color
        0xff8c00ffu,
};

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Map<Color, Color> p_rekordboxMarkerColorFor {
        // -- These are Serato marker colors and their best match in rekordbox.
        { 0xcc00ccffu, 0xde44cfffu },
        { 0xcc0088ffu, 0xff127bffu },
        { 0xcc0044ffu, 0xe62828ffu },
        { 0xcc0000ffu, 0xe62828ffu },
        { 0xcc4400ffu, 0xe0641bffu },
        { 0xcc8800ffu, 0xe0641bffu },
        { MarkerColor::YellowColor, 0xc3af04ffu },
        { 0x88cc00ffu, 0xa5e116ffu },
        { 0x44cc00ffu, 0x28e214ffu },
        { MarkerColor::GreenColor, 0x28e214ffu },
        { 0x00cc44ffu, 0x28e214ffu },
        { 0x00cc88ffu, 0x10b176ffu },
        { MarkerColor::TurquoiseColor, 0x0072ffffu },
        { 0x0088ccffu, 0x50b4ffffu },
        { 0x0044ccffu, 0x305affffu },
        { 0x0000ccffu, 0x6473ffffu },
        { 0x4400ccffu, 0x6473ffffu },
        { 0x8800ccffu, 0xb432ffffu },

        // -- These are standard rekordbox marker colors which don't need translating
        { 0xde44cfffu, 0xde44cfffu },
        { 0xff127bffu, 0xff127bffu },
        { 0xe62828ffu, 0xe62828ffu },
        { 0xe0641bffu, 0xe0641bffu },
        { MarkerColor::OrangeColor, MarkerColor::OrangeColor },
        { 0xc3af04ffu, 0xc3af04ffu },
        { 0xb4be04ffu, 0xb4be04ffu },
        { 0xa5e116ffu, 0xa5e116ffu },
        { 0x28e214ffu, 0x28e214ffu },
        { 0x10b176ffu, 0x10b176ffu },
        { 0x1fa392ffu, 0x1fa392ffu },
        { 0x50b4ffffu, 0x50b4ffffu },
        { 0x0072ffffu, 0x0072ffffu },
        { 0x305affffu, 0x305affffu },
        { 0x6473ffffu, 0x6473ffffu },
        { 0xaa72ffffu, 0xaa72ffffu },
        { 0xb432ffffu, 0xb432ffffu },
};

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Map<Color, Color> p_seratoMarkerColorFor {
        // -- These are rekordbox marker colors and their best match in Serato.
        { 0xde44cfffu, 0xcc00ccffu },
        { 0xff127bffu, 0xcc0088ffu },
        { 0xe62828ffu, 0xcc0000ffu },
        { 0xe0641bffu, 0xcc4400ffu },
        { MarkerColor::OrangeColor, 0xcc8800ffu },
        { 0xc3af04ffu, 0xcccc00ffu },
        { 0xb4be04ffu, 0xcccc00ffu },
        { 0xa5e116ffu, 0x88cc00ffu },
        { 0x28e214ffu, 0x00cc00ffu },
        { 0x10b176ffu, 0x00cc88ffu },
        { 0x1fa392ffu, 0x00cc88ffu },
        { 0x50b4ffffu, 0x0088ccffu },
        { 0x0072ffffu, 0x00ccccffu },
        { 0x305affffu, 0x0044ccffu },
        { 0x6473ffffu, 0x0000ccffu },
        { 0xaa72ffffu, 0x8800ccffu },
        { 0xb432ffffu, 0x8800ccffu },

        // -- These are standard Serato marker colors which don't need translating
        { 0xcc00ccffu, 0xcc00ccffu },
        { 0xcc0088ffu, 0xcc0088ffu },
        { 0xcc0044ffu, 0xcc0044ffu },
        { 0xcc0000ffu, 0xcc0000ffu },
        { 0xcc4400ffu, 0xcc4400ffu },
        { 0xcc8800ffu, 0xcc8800ffu },
        { MarkerColor::YellowColor, MarkerColor::YellowColor },
        { 0x88cc00ffu, 0x88cc00ffu },
        { 0x44cc00ffu, 0x44cc00ffu },
        { MarkerColor::GreenColor, MarkerColor::GreenColor },
        { 0x00cc44ffu, 0x00cc44ffu },
        { 0x00cc88ffu, 0x00cc88ffu },
        { MarkerColor::TurquoiseColor, MarkerColor::TurquoiseColor },
        { 0x0088ccffu, 0x0088ccffu },
        { 0x0044ccffu, 0x0044ccffu },
        { 0x0000ccffu, 0x0000ccffu },
        { 0x4400ccffu, 0x4400ccffu },
        { 0x8800ccffu, 0x8800ccffu },
};

} }

using namespace NxA;
using namespace NxA::Common;

// -- Class Methods

Optional<Color> MarkerColor::maybeColorForMarkerColorAndMaybeHotCueNumberWhenExportedTo(const Color& color,
                                                                                        Optional<count> maybeIndex,
                                                                                        Common::Collection::Type collectionType,
                                                                                        MarkerColor::IsALoopMarker isALoopMarker,
                                                                                        Common::Marker::Flags flags)
{
    switch(collectionType) {
        case Common::Collection::Type::Serato: {
            if (!maybeIndex.isValid() || (isALoopMarker == MarkerColor::IsALoopMarker::Yes)) {
                return nothing;
            }

            if (color.asRGBA()) {
                auto maybeColor = p_seratoMarkerColorFor.maybeValueForKey(color);
                if (maybeColor.isValid()) {
                    return maybeColor;
                }
            }

            auto& index = *maybeIndex;
            if (index >= sizeof(p_seratoDefaultMarkerColors)) {
                // -- We shouldn't have hotcues beyond the supported number in Serato but we check just in case.
                return nothing;
            }

            // -- If the marker doesn't have a color, we use the default Serato one depending on the hot cue index
            return p_seratoDefaultMarkerColors[index];
        }
        case Common::Collection::Type::rekordbox: {
            if (color.asRGBA()) {
                return p_rekordboxMarkerColorFor.maybeValueForKey(color);
            }

            return (isALoopMarker == MarkerColor::IsALoopMarker::Yes) ? p_rekordboxDefaultLoopMarkerColor : p_rekordboxDefaultMarkerColor;
        }
        case Common::Collection::Type::Traktor: {
            if (isALoopMarker == MarkerColor::IsALoopMarker::Yes) {
                return MarkerColor::GreenColor;
            }

            if (flags.has(Common::Marker::Flag::IsALoadMarker)) {
                return MarkerColor::YellowColor;
            }

            if (flags.has(Common::Marker::Flag::IsAFadeInMarker) || flags.has(Common::Marker::Flag::IsAFadeOutMarker)) {
                return MarkerColor::OrangeColor;
            }

            if (maybeIndex.isValid()) {
                return MarkerColor::TurquoiseColor;
            }

            return nothing;
        }
        default: {
            // -- For now all other collections just return no color
            return nothing;
        }
    }
}
