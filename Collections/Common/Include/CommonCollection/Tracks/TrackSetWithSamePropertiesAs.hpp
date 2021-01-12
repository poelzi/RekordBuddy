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

using namespace NxA;
using namespace NxA::Common;

// -- Instance Methods

// -- This template is separated out so that it can be used in unit tests too.

template <class T>
    void MutableTrack::setWithSamePropertiesAs(const T& other)
    {
        this->setTitle(other.maybeTitle());
        this->setAlbum(other.maybeAlbum());
        this->setAlbumTrackCount(other.maybeAlbumTrackCount());
        this->setComments(other.maybeComments());
        this->setGenres(other.genres());
        this->setGrouping(other.maybeGrouping());
        this->setMixName(other.maybeMixName());
        this->setRecordLabel(other.maybeRecordLabel());
        this->setTags(other.tags());
        this->setArtists(other.artists());
        this->setProducers(other.producers());
        this->setRemixers(other.remixers());
        this->setDateAdded(other.maybeDateAdded());
        this->setDateReleased(other.maybeDateReleased());
        this->setBeatGridLocked(other.maybeBeatGridLocked().valueOr(false));
        this->setBitDepthInBits(other.maybeBitDepthInBits());
        this->setBitRateInKiloBitsPerSecond(other.maybeBitRateInKiloBitsPerSecond());
        this->setBeatsPerMinute(other.maybeBeatsPerMinute());
        this->setColor(other.maybeColor());
        this->setFileSizeInBytes(other.maybeFileSizeInBytes());
        this->setFileType(other.fileType());
        this->setMusicalKeys(other.musicalKeys());
        this->setLengthInSeconds(other.maybeLengthInSeconds());
        this->setTrackNumber(other.maybeTrackNumber());
        this->setDiscNumber(other.maybeDiscNumber());
        this->setPlayCount(other.maybePlayCount());
        this->setRating(other.maybeRating());
        this->setSampleRateInHertz(other.maybeSampleRateInHertz());

        auto otherCollection = other.collection();
        if (otherCollection->providesCuePointsAndBeatGrid()) {
            auto thisType = this->collection()->type();
            auto otherType = otherCollection->type();

            // -- Rekord Buddy's db caches these offsets so we prefer to use that if it's available.
            auto thisCollectionIsRekordBuddy = (thisType == Collection::Type::RekordBuddy);
            auto maybeOffsetToAddInSeconds = thisCollectionIsRekordBuddy ?
                                             Track::maybeOffsetToAddInSecondsWhenImportingTrackFrom(*this, otherType) :
                                             Track::maybeOffsetToAddInSecondsWhenImportingTrackFrom(other, otherType);
            auto maybeOffsetToSubtractInSeconds = thisCollectionIsRekordBuddy ?
                                                  Track::maybeOffsetToAddInSecondsWhenImportingTrackFrom(*this, Collection::Type::RekordBuddy) :
                                                  Track::maybeOffsetToAddInSecondsWhenImportingTrackFrom(other, thisType);
            if (maybeOffsetToSubtractInSeconds.isValid()) {
                if (maybeOffsetToAddInSeconds.isValid()) {
                    maybeOffsetToAddInSeconds = *maybeOffsetToAddInSeconds - *maybeOffsetToSubtractInSeconds;
                }
                else {
                    maybeOffsetToAddInSeconds = maybeOffsetToSubtractInSeconds->asNegative();
                }
            }

#if defined(NXA_MARKER_COPY_DEBUG_INFO)
            NXA_BETA_LOG_WITH_FORMAT("Copying track '%s' from %s to %s.\n",
                                     this->absoluteFilePath().asEncodedString().asUTF8(),
                                     Common::Collection::nameForType(otherType).asUTF8(),
                                     Common::Collection::nameForType(thisType).asUTF8());
#endif
            this->setMarkersAndMaybeAddOffsetInSeconds(other.markers(), maybeOffsetToAddInSeconds);
        }

        // -- This has to be called last in order to let any collection finalize any modification to the track.
        this->markAsModifiedNow();
    }
