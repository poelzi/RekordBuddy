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

#include <CommonCollection/Tracks/Property.hpp>

#include <CommonCollection/Tracks/MusicalKey.hpp>

namespace NxA { namespace Common {

// -- Translation from a TypeID to the name as a string.

static Map<Property::TypeID, String> p_nameForPropertyType
{
    // -- These are stored in user data and should not be modified.
    { Property::TypeID::ArtistCredit, "Artist" },
    { Property::TypeID::Comments, "Comments" },
    { Property::TypeID::Genre, "Genre" },
    { Property::TypeID::Grouping, "Grouping" },
    { Property::TypeID::MixName, "Mix Name" },
    { Property::TypeID::ProducerCredit, "Producer" },
    { Property::TypeID::RemixerCredit, "Remixer" },
    { Property::TypeID::RecordLabel, "Record Label" },
    { Property::TypeID::Tag, "Tag" },
    { Property::TypeID::Title, "Title" },
    { Property::TypeID::DateAdded, "Date Added" },
    { Property::TypeID::DateReleased, "Date Released" },
    { Property::TypeID::BeatGridLockedFlag, "Grid Locked" },
    { Property::TypeID::BitDepthInBits, "Bit Depth" },
    { Property::TypeID::BitRateInKiloBitsPerSecond, "Bit Rate" },
    { Property::TypeID::Color, "Color" },
    { Property::TypeID::FileSize, "File Size" },
    { Property::TypeID::MusicalKey, "Musical Key" },
    { Property::TypeID::LengthInSeconds, "Length" },
    { Property::TypeID::TrackNumber, "Track Number" },
    { Property::TypeID::PlayCount, "Play Count" },
    { Property::TypeID::Rating, "Rating" },
    { Property::TypeID::SampleRateInHertz, "Sample Rate" },
    { Property::TypeID::FileType, "File Type" },
    { Property::TypeID::BeatsPerMinute, "BPM" },
    { Property::TypeID::LastModifiedOn, "Date Modified" },
    { Property::TypeID::RelativeFilePath, "Relative Filepath" },
    { Property::TypeID::NumberOfTracks, "Number of Tracks" },
    { Property::TypeID::Album, "Album" },
};

} }

using namespace NxA;
using namespace NxA::Common;

// -- Class Methods

String Property::nameForType(Property::TypeID type)
{
    return p_nameForPropertyType[type];
}
