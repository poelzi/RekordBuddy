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

#include <CommonCollection/Tracks/TrackColor.hpp>

#include <Base/Array.hpp>
#include <Base/Map.hpp>

namespace NxA { namespace Common {

// -- Static Variables

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Map<Color, Color> p_rekordboxTrackColorFor {
        // -- These are Serato track colors and their best match in rekordbox.
        { 0xff99ffffu, 0xFF007Fffu },
        { 0xff99ddffu, 0xFF007Fffu },
        { 0xff99bbffu, 0xFF007Fffu },
        { 0xff9999ffu, TrackColor::RedColor },
        { 0xffbb99ffu, 0xFFA500ffu },
        { 0xffdd99ffu, 0xFFA500ffu },
        { 0xffff99ffu, 0xFFFF00ffu },
        { 0xddff99ffu, 0x00FF00ffu },
        { 0xbbff99ffu, 0x00FF00ffu },
        { 0x99ff99ffu, 0x00FF00ffu },
        { 0x99ffbbffu, 0x00FF00ffu },
        { 0x99ffddffu, TrackColor::TurquoiseColor },
        { 0x99ffffffu, TrackColor::TurquoiseColor },
        { 0x99ddffffu, TrackColor::TurquoiseColor },
        { 0x99bbffffu, TrackColor::BlueColor },
        { 0x9999ffffu, TrackColor::PurpleColor },
        { 0xbb99ffffu, TrackColor::PurpleColor },
        { 0xdd99ffffu, TrackColor::PurpleColor },

        // -- These are Traktor track colors and their best match in rekordbox.
        { TrackColor::MagentaColor, 0xFF007Fffu },
        { 0xfd4a4affu, TrackColor::RedColor },
        { TrackColor::OrangeColor, 0xFFA500ffu },
        { TrackColor::YellowColor, 0xFFFF00ffu },
        { TrackColor::GreenColor, 0x00FF00ffu },
        { 0x3f8bfeffu, TrackColor::BlueColor },
        { 0xad65ffffu, TrackColor::PurpleColor },

        // -- These are standard rekordbox track colors which don't need translating
        { 0xFF007Fffu, 0xFF007Fffu },
        { TrackColor::RedColor, TrackColor::RedColor },
        { 0xFFA500ffu, 0xFFA500ffu },
        { 0xFFFF00ffu, 0xFFFF00ffu },
        { 0x00FF00ffu, 0x00FF00ffu },
        { TrackColor::TurquoiseColor, TrackColor::TurquoiseColor },
        { TrackColor::BlueColor, TrackColor::BlueColor },
        { TrackColor::PurpleColor, TrackColor::PurpleColor },
};

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Map<Color, Color> p_seratoTrackColorFor {
        // -- These are rekordbox track colors and their best match in Serato.
        { 0xf971f4ffu, 0xff99ffffu },
        { TrackColor::RedColor, 0xff9999ffu },
        { 0xfc9d3fffu, 0xffdd99ffu },
        { 0xfbe14affu, 0xffff99ffu },
        { 0x00e034ffu, 0x99ff99ffu },
        { TrackColor::TurquoiseColor, 0x99ddffffu },
        { TrackColor::BlueColor, 0x99bbffffu },
        { TrackColor::PurpleColor, 0xdd99ffffu },

        // -- These are Traktor track colors and their best match in Serato.
        { TrackColor::MagentaColor, 0xff99ffffu },
        { 0xfd4a4affu, 0xff9999ffu },
        { TrackColor::OrangeColor, 0xffdd99ffu },
        { TrackColor::YellowColor, 0xffff99ffu },
        { TrackColor::GreenColor, 0x99ff99ffu },
        { 0x3f8bfeffu, 0x99ddffffu },
        { 0xad65ffffu, 0xbb99ffffu },

        // -- These are standard Serato track colors which don't need translating
        { 0xff99ffffu, 0xff99ffffu },
        { 0xff99ddffu, 0xff99ddffu },
        { 0xff99bbffu, 0xff99bbffu },
        { 0xff9999ffu, 0xff9999ffu },
        { 0xffbb99ffu, 0xffbb99ffu },
        { 0xffdd99ffu, 0xffdd99ffu },
        { 0xffff99ffu, 0xffff99ffu },
        { 0xddff99ffu, 0xddff99ffu },
        { 0xbbff99ffu, 0xbbff99ffu },
        { 0x99ff99ffu, 0x99ff99ffu },
        { 0x99ffbbffu, 0x99ffbbffu },
        { 0x99ffddffu, 0x99ffddffu },
        { 0x99ffffffu, 0x99ffffffu },
        { 0x99ddffffu, 0x99ddffffu },
        { 0x99bbffffu, 0x99bbffffu },
        { 0x9999ffffu, 0x9999ffffu },
        { 0xbb99ffffu, 0xbb99ffffu },
        { 0xdd99ffffu, 0xdd99ffffu },
        { 0x999999ffu, 0x999999ffu },
        { 0xffffffffu, 0xffffffffu },
        { 0xbbbbbbffu, 0xbbbbbbffu },
};

#if !defined(NXA_BUILD_FOR_TESTING)
static
#endif
Map<Color, Color> p_traktorTrackColorFor {
        // -- These are rekordbox track colors and their best match in Traktor.
        { 0xf971f4ffu, TrackColor::MagentaColor },
        { TrackColor::RedColor, 0xfd4a4affu },
        { 0xfc9d3fffu, TrackColor::OrangeColor },
        { 0xfbe14affu, TrackColor::YellowColor },
        { 0x00e034ffu, TrackColor::GreenColor },
        { TrackColor::TurquoiseColor, 0x3f8bfeffu },
        { TrackColor::BlueColor, 0x3f8bfeffu },
        { TrackColor::PurpleColor, 0xad65ffffu },

        // -- These are Serato track colors and their best match in Traktor.
        { 0xff99ffffu, TrackColor::MagentaColor },
        { 0xff99ddffu, TrackColor::MagentaColor },
        { 0xff99bbffu, TrackColor::MagentaColor },
        { 0xff9999ffu, 0xfd4a4affu },
        { 0xffbb99ffu, 0xfd4a4affu },
        { 0xffdd99ffu, TrackColor::OrangeColor },
        { 0xffff99ffu, TrackColor::YellowColor },
        { 0xddff99ffu, TrackColor::GreenColor },
        { 0xbbff99ffu, TrackColor::GreenColor },
        { 0x99ff99ffu, TrackColor::GreenColor },
        { 0x99ffbbffu, TrackColor::GreenColor },
        { 0x99ffddffu, TrackColor::GreenColor },
        { 0x99ffffffu, 0x3f8bfeffu },
        { 0x99ddffffu, 0x3f8bfeffu },
        { 0x99bbffffu, 0x3f8bfeffu },
        { 0x9999ffffu, 0xad65ffffu },
        { 0xbb99ffffu, 0xad65ffffu },
        { 0xdd99ffffu, 0xad65ffffu },

        // -- These are standard Traktor track colors which don't need translating
        { TrackColor::MagentaColor, TrackColor::MagentaColor },
        { 0xfd4a4affu, 0xfd4a4affu },
        { TrackColor::OrangeColor, TrackColor::OrangeColor },
        { TrackColor::YellowColor, TrackColor::YellowColor },
        { TrackColor::GreenColor, TrackColor::GreenColor },
        { 0x3f8bfeffu, 0x3f8bfeffu },
        { 0xad65ffffu, 0xad65ffffu },
};

} }

using namespace NxA;
using namespace Common;

// -- Class Methods

Optional<Color> TrackColor::maybeColorForTrackColorWhenExportedTo(const Color& color, Common::Collection::Type collectionType)
{
    switch(collectionType) {
        case Common::Collection::Type::Serato: {
            return p_seratoTrackColorFor.maybeValueForKey(color);
        }
        case Common::Collection::Type::rekordbox: {
            return p_rekordboxTrackColorFor.maybeValueForKey(color);
        }
        case Common::Collection::Type::Traktor: {
            return p_traktorTrackColorFor.maybeValueForKey(color);
        }
        default: {
            // -- For now all other collections just return no color
            return nothing;
        }
    }
}
