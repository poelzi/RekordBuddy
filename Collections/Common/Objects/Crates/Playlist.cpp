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

#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Tracks/Track.hpp>
#include <CommonCollection/Tracks/TrackPredicate.hpp>

using namespace NxA;
using namespace NxA::Common;

// -- Instance Methods

count Playlist::numberOfCratesAndTrackEntriesContainedWithin() const
{
    return Playlist::numberOfCratesAndTrackEntriesContainedIn(*this);
}

boolean Playlist::hasTracksNotOnVolume(const Volume& volume) const
{
    return Playlist::hasTracksNotOnVolume(*this, volume);
}

boolean Playlist::hasMovieTracks() const
{
    return Playlist::hasMovieTracks(*this);
}

String MutablePlaylist::nextAvailableNameForThisIfAddedTo(const MutableFolder& folder) const
{
    return folder.nextAvailableNameForPlaylistNamed(this->name());
}

count MutablePlaylist::numberOfCratesAndTrackEntriesContainedWithin() const
{
    return Playlist::numberOfCratesAndTrackEntriesContainedIn(*this);
}

boolean MutablePlaylist::hasTracksNotOnVolume(const Volume& volume) const
{
    return Playlist::hasTracksNotOnVolume(*this, volume);
}

boolean MutablePlaylist::hasMovieTracks() const
{
    return Playlist::hasMovieTracks(*this);
}

template<class T>
    Optional<count> MutablePlaylist::p_addTrackAtIndex(NotNull<T*> track,
                                                       count index,
                                                       AndUpdateTracks updateTrack,
                                                       Common::Track::TrackIsOnSameVolume trackIsOnSameVolume)
    {
        NXA_ASSERT_TRUE(index <= this->numberOfTracks());

        Optional<NotNull<MutableTrack*>> newOrExistingTrack;

        auto collection = this->collection();
        if (collection->mustHaveTracksOnTheSameVolume()) {
            auto maybeNewRelativeFilePath = Common::Track::maybeFilePathForTrackRelativeToVolume(track,
                                                                                                 collection->volume(),
                                                                                                 trackIsOnSameVolume);
            if (!maybeNewRelativeFilePath.isValid()) {
                // -- Something went wrong, we can't make a relative path for that collection that should accept tracks from all volumes.
                return nothing;
            }

            auto& relativeFilePath = *maybeNewRelativeFilePath;
            auto maybeExistingTrack = collection->maybeExistingTrackWithRelativeFilePath(relativeFilePath);
            newOrExistingTrack = maybeExistingTrack.isValid() ? NotNull<MutableTrack*>{ *maybeExistingTrack }
                                                              : collection->trackWithRelativeFilePath(relativeFilePath);
        }
        else {
            auto absoluteFilePath = track->absoluteFilePath();
            auto maybeExistingTrack = collection->maybeExistingTrackWithAbsoluteFilePath(absoluteFilePath);
            newOrExistingTrack = maybeExistingTrack.isValid() ? NotNull<MutableTrack*>{ *maybeExistingTrack }
                                                              : collection->trackWithAbsoluteFilePath(absoluteFilePath);
        }

        if ((updateTrack == AndUpdateTracks::Yes) && !Common::MutableCollection::isEqual(*collection, *track->collection())) {
            (*newOrExistingTrack)->setWithSamePropertiesAs(*track);
        }

        return this->p_addExistingTrackAtIndex((*newOrExistingTrack), index);
    }

Optional<count> MutablePlaylist::addTrackAtIndex(NotNull<const Common::Track*> track,
                                                 count index,
                                                 AndUpdateTracks updateTrack,
                                                 Common::Track::TrackIsOnSameVolume trackIsOnSameVolume)
{
    return this->p_addTrackAtIndex(track, index, updateTrack, trackIsOnSameVolume);
}

Optional<count> MutablePlaylist::addTrackAtIndex(NotNull<Common::MutableTrack*> track,
                                                 count index,
                                                 AndUpdateTracks updateTrack,
                                                 Common::Track::TrackIsOnSameVolume trackIsOnSameVolume)
{
    return this->p_addTrackAtIndex(track, index, updateTrack, trackIsOnSameVolume);
}

void MutablePlaylist::removeTrackWithRelativeFilePath(const FilePath& filePath)
{
    auto numberOfTracksLeft = this->numberOfTracks();

    for (count index = 0; index < numberOfTracksLeft;) {
        if (this->trackAtIndex(index)->relativeFilePath() == filePath) {
            this->removeTrackAtIndex(index);

            --numberOfTracksLeft;
        }
        else {
            ++index;
        }
    }
}

void MutablePlaylist::removeTrackWithAbsoluteFilePath(const FilePath& filePath)
{
    auto numberOfTracksLeft = this->numberOfTracks();

    for (count index = 0; index < numberOfTracksLeft;) {
        if (this->trackAtIndex(index)->absoluteFilePath() == filePath) {
            this->removeTrackAtIndex(index);

            --numberOfTracksLeft;
        }
        else {
            ++index;
        }
    }
}
