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


#include <TraktorCollection/Tracks/Track.hpp>

using namespace NxA;
using namespace NxA::Traktor;

// -- instance Methods

FilePath MutableTrack::absoluteFilePath() const
{
    if (!this->p_maybeLocation.isValid()) {
        auto maybeLocationNode = this->p_maybeLocationNode();
        NXA_ASSERT_TRUE(maybeLocationNode.isValid());

        auto maybeDirectory = maybeLocationNode->maybeStringValueForAttributeNamed("DIR");
        auto maybeFile = maybeLocationNode->maybeStringValueForAttributeNamed("FILE");
        NXA_ASSERT_TRUE(maybeDirectory.isValid() && maybeDirectory.isValid());

        auto maybeVolumeAsString = maybeLocationNode->maybeStringValueForAttributeNamed("VOLUME");
        if(!maybeVolumeAsString.isValid() || (maybeVolumeAsString->length() == 0)) {
            auto maybeVolumeID = maybeLocationNode->maybeStringValueForAttributeNamed("VOLUMEID");
            if (maybeVolumeID.isValid()) {
                auto maybeVolume = Volume::maybeVolumeForVolumeName(*maybeVolumeID);
                if (maybeVolume.isValid()) {
                    NXA_ASSERT_TRUE(maybeDirectory->hasPrefix(*maybeVolumeID));
                    maybeDirectory = maybeDirectory->subString(maybeVolumeID->length());
                    maybeVolumeAsString = maybeVolume->asFilePath().asEncodedString();
                }
            }
        }

        NXA_ASSERT_TRUE(maybeVolumeAsString.isValid());

        auto traktorLocation = maybeVolumeAsString->stringByAppending(maybeDirectory->stringByAppending(*maybeFile));
        this->p_maybeLocation = MutableTrack::p_traktorPathToAbsoluteFilePath(traktorLocation);
    }

    return *this->p_maybeLocation;
}

String MutableTrack::trackFilePathAsUsedInTraktorPlaylistEntries() const
{
    // -- This is just a first pass at what the return value should be.
    // -- On macOS Traktor has a lot of legacy weirdness baked into how it deals with different paths and different setup.
    // -- I'm assuming we'll find the same things on Windows over time.
    auto maybeLocationNode = this->p_maybeLocationNode();
    NXA_ASSERT_TRUE(maybeLocationNode.isValid());

    auto maybeDirectory = maybeLocationNode->maybeStringValueForAttributeNamed("DIR");
    auto maybeFile = maybeLocationNode->maybeStringValueForAttributeNamed("FILE");
    auto maybeVolume = maybeLocationNode->maybeStringValueForAttributeNamed("VOLUME");

    return maybeVolume->stringByAppending(*maybeDirectory).stringByAppending(*maybeFile);
}
