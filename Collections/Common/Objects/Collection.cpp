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

#include <CommonCollection/Collection.hpp>

#include <CommonCollection/Crates/Playlist.hpp>

using namespace NxA;
using namespace NxA::Common;

// -- Class Methods

void Collection::makeSureStringsFromPreferenceKeyAreNotAlreadyInThenAddOnesThatAreNot(UserPreferences& userPreferences, const String& preferenceKey, MutableArray<String>& currentStrings)
{
    auto preferenceStrings = userPreferences.arrayOfStringsForKey(preferenceKey);
    Optional<MutableArray<String>> maybeCorrectedPreferenceStrings;

    for (auto&& stringToTest : preferenceStrings) {
        if (!currentStrings.contains(stringToTest)) {
            continue;
        }

        if (!maybeCorrectedPreferenceStrings.isValid()) {
            maybeCorrectedPreferenceStrings = MutableArray<String>{ preferenceStrings };
        }

        maybeCorrectedPreferenceStrings->remove(stringToTest);
    }

    if (maybeCorrectedPreferenceStrings.isValid()) {
        currentStrings.appendObjectsFrom(*maybeCorrectedPreferenceStrings);
        userPreferences.setArrayOfStringsForKey(*maybeCorrectedPreferenceStrings, preferenceKey);
    }
    else {
        currentStrings.appendObjectsFrom(preferenceStrings);
    }
}

// -- Instance Methods

boolean Collection::hasTracksNotOnVolume(const Volume& volume) const
{
    for (auto&& track : this->tracks()->tracks()) {
        if (track->volume() != volume) {
            return true;
        }
    }

    return false;
}

boolean MutableCollection::hasTracksNotOnVolume(const Volume& volume) const
{
    for (auto&& track : this->tracks()->tracks()) {
        if (track->volume() != volume) {
            return true;
        }
    }

    return false;
}

NotNull<Common::MutableTrack*> MutableCollection::trackWithRelativeFilePath(const FilePath& relativeFilePath)
{
    // -- If the collection does not accepts tracks on other volumes then it must override this method.
    NXA_ASSERT_FALSE(this->mustHaveTracksOnTheSameVolume());

    return this->trackWithAbsoluteFilePath(FilePath::filePathByJoiningPaths(this->volume(), relativeFilePath));
}

Optional<NotNull<Common::MutableTrack*>> MutableCollection::maybeExistingTrackWithRelativeFilePath(const FilePath& relativeFilePath)
{
    // -- If the collection does not accepts tracks on other volumes then it must override this method.
    NXA_ASSERT_FALSE(this->mustHaveTracksOnTheSameVolume());

    return this->maybeExistingTrackWithAbsoluteFilePath(FilePath::filePathByJoiningPaths(this->volume(), relativeFilePath));
}

NotNull<MutableTrack*> MutableCollection::trackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    // -- If the collection accepts tracks on other volumes then it must override this method.
    NXA_ASSERT_TRUE(this->mustHaveTracksOnTheSameVolume());

    auto volume = Volume{ absoluteFilePath };
    NXA_ASSERT_TRUE(volume == this->volume());

    return this->trackWithRelativeFilePath(*absoluteFilePath.maybeRelativeToVolume(volume));
}

Optional<NotNull<MutableTrack*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    // -- If the collection accepts tracks on other volumes then it must override this method.
    NXA_ASSERT_TRUE(this->mustHaveTracksOnTheSameVolume());

    auto volume = Volume{ absoluteFilePath };
    if (volume == this->volume()) {
        return this->maybeExistingTrackWithRelativeFilePath(*absoluteFilePath.maybeRelativeToVolume(volume));
    }

    return nothing;
}
