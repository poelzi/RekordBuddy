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

#include <Foundation/Foundation.h>

using namespace NxA;
using namespace NxA::Traktor;

// -- Instance Methods

FilePath MutableTrack::absoluteFilePath() const
{
    if (!this->p_maybeLocation.isValid()) {
        auto maybeLocationNode = this->p_maybeLocationNode();
        NXA_ASSERT_TRUE(maybeLocationNode.isValid());

        auto maybeDirectory = maybeLocationNode->maybeStringValueForAttributeNamed("DIR");
        auto maybeFile = maybeLocationNode->maybeStringValueForAttributeNamed("FILE");
        auto maybeVolume = maybeLocationNode->maybeStringValueForAttributeNamed("VOLUME");
        NXA_ASSERT_TRUE(maybeDirectory.isValid() && maybeDirectory.isValid() && maybeVolume.isValid());

        String traktorLocation;

        if (*maybeVolume == Volume::homeVolumeName()) {
            traktorLocation = maybeDirectory->stringByAppending(*maybeFile);
        }
        else {
            auto maybeVolumeID = maybeLocationNode->maybeStringValueForAttributeNamed("VOLUMEID");
            if (maybeVolumeID == maybeVolume) {
                if (maybeVolume == Platform::maybeUsername()) {
                    traktorLocation = "/:Users/:"_String.stringByAppending(maybeVolume->stringByAppending(maybeDirectory->stringByAppending(*maybeFile)));
                }
                else if (!maybeVolume->length()) {
                    traktorLocation = maybeDirectory->stringByAppending(*maybeFile);
                }
            }
            else if (*maybeVolume == "Users"_String) {
                traktorLocation = "/:"_String.stringByAppending(maybeVolume->stringByAppending(*maybeDirectory).stringByAppending(*maybeFile));
            }

            if (!traktorLocation.length()) {
                auto maybeDoubleForwardSlashPosition = maybeDirectory->maybeIndexOfFirstOccurenceOf("/:/:"_String);
                if (maybeDoubleForwardSlashPosition.isValid()) {
                    // -- This seems to be a case where Traktor stores the app name and version in the path so we remove it.
                    maybeDirectory = maybeDirectory->subString(*maybeDoubleForwardSlashPosition + 2);
                }

                if (maybeVolume->length()) {
                    traktorLocation = "/:Volumes/:"_String.stringByAppending(maybeVolume->stringByAppending(*maybeDirectory).stringByAppending(*maybeFile));
                }
                else {
                    traktorLocation = maybeDirectory->stringByAppending(*maybeFile);
                }
            }
        }

        this->p_maybeLocation = MutableTrack::p_traktorPathToAbsoluteFilePath(traktorLocation);
    }

    return *this->p_maybeLocation;
}

String MutableTrack::trackFilePathAsUsedInTraktorPlaylistEntries() const
{
    auto maybeLocationNode = this->p_maybeLocationNode();
    NXA_ASSERT_TRUE(maybeLocationNode.isValid());

    auto maybeDirectory = maybeLocationNode->maybeStringValueForAttributeNamed("DIR");
    auto maybeFile = maybeLocationNode->maybeStringValueForAttributeNamed("FILE");
    auto maybeVolume = maybeLocationNode->maybeStringValueForAttributeNamed("VOLUME");
    NXA_ASSERT_TRUE(maybeDirectory.isValid() && maybeDirectory.isValid() && maybeVolume.isValid());

    return maybeVolume->stringByAppending(maybeDirectory->stringByAppending(*maybeFile));
}
