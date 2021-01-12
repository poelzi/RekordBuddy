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

#include <CommonCollection/Tracks/MusicalKey.hpp>
#include <CommonCollection/Markers/Markers.hpp>

#include <CommonCollection/Tracks/TrackRating.hpp>
#include <CommonCollection/Markers/MarkerValidation.hpp>
#include <CommonCollection/Markers/MarkerOffset.hpp>

#include <TrackFiles/TrackFile.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class Collection;
class MutableCollection;

// -- Public Interface
class Track : public Uncopyable
{
public:
    // -- Constants
    enum class Error : byte {
        // -- These are stored in user data and cannot be re-ordered or modified.
        // -- They all need to be unique.
        TrackFileCouldNotBeUpdated,
    };
    enum class TrackIsOnSameVolume {
        DontKnow,
        Yes
    };
    enum class Property {
        // -- These are stored in user data and should not be re-ordered or modified.
        // -- New values can be added to the list though.
        Title,
        Artists,
        Remixers,
        Producers,
        Comments,
        Tags,
        MusicalKeys,
        Genres,
        RecordLabel,
        Rating,
        MixName,
        PlayCount,
        BeatsPerMinute,
        Length,
        BitRate,
        BitDepth,
        FileLocation,
        SampleRate,
        DateAdded,
        DateModified,
        DateReleased,
        AlbumTitle,
        TrackCount,
        TrackNumber,
        Grouping,
        FileType,
        FileSize,
        AlbumArt,

        // -- Add new properties here.

        LastProperty,

        // -- This is required for static assertions.
        LastFlag,
    };
    using PropertyFlags = Flags<Property>;

    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const Track& first, const T& second)
        {
            return (first.collection() == second.collection()) && (first.relativeFilePath() == second.relativeFilePath());
        }
    template <class T>
        inline static FilePath absolutePathFor(const T& track)
        {
            return FilePath::filePathByJoiningPaths(track.volume(), track.relativeFilePath());
        }
    template <class T>
        inline static String artistsAsStringFor(const T& track)
        {
            return String::stringByJoiningArrayWithString(track.artists(), track.collection()->artistsSeparator());
        }
    template <class T>
        inline static String producersAsStringFor(const T& track)
        {
            return String::stringByJoiningArrayWithString(track.producers(), track.collection()->artistsSeparator());
        }
    template <class T>
        inline static String remixersAsStringFor(const T& track)
        {
            return String::stringByJoiningArrayWithString(track.remixers(), track.collection()->artistsSeparator());
        }
    template <class T>
        inline static String tagsAsStringFor(const T& track)
        {
            return String::stringByJoiningArrayWithString(track.tags(), ", "_String);
        }
    template <class T>
        inline static String genresAsStringFor(const T& track)
        {
            return String::stringByJoiningArrayWithString(track.genres(), track.collection()->genresSeparator());
        }
    template <class T>
        inline static String musicalKeysAsStringFor(const T& track)
        {
            return String::stringByJoiningArrayWithString(track.musicalKeys(), track.collection()->musicalKeysSeparator());
        }
    template <class T>
        inline static String ratingAsStringFor(const T& track)
        {
            return track.maybeRating().isValid() ? track.maybeRating()->asString() : String{ };
        }
    template <class T>
        inline static Optional<DecimalNumber> maybeOffsetToAddInSecondsWhenImportingTrackFrom(const T& track, Common::Collection::Type type)
        {
            return Common::MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(track.absoluteFilePath(), type);
        }
    template <class T>
        inline static Optional<FilePath> maybeFilePathForTrackRelativeToVolume(const T& track, const Volume& volume, TrackIsOnSameVolume trackIsOnSameVolume)
        {
            if (trackIsOnSameVolume == TrackIsOnSameVolume::Yes) {
                return { track->relativeFilePath() };
            }

            return track->absoluteFilePath().maybeRelativeToVolume(volume);
        }
    template <class T>
        inline static boolean isAMovieTrack(const T& track)
        {
            return TrackFile::isAMovieTrack(track.absoluteFilePath());
        }

    // -- Constructors & Destructors
    virtual ~Track() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual Volume volume() const = 0;
    virtual FilePath relativeFilePath() const = 0;
    virtual FilePath absoluteFilePath() const = 0;

    virtual Time lastModificationTime() const = 0;

    virtual Optional<String> maybeTitle() const = 0;
    virtual Optional<String> maybeAlbum() const = 0;
    virtual Optional<count> maybeAlbumTrackCount() const = 0;
    virtual Optional<String> maybeComments() const = 0;
    virtual Array<String> genres() const = 0;
    virtual Optional<String> maybeGrouping() const = 0;
    virtual Optional<String> maybeMixName() const = 0;
    virtual Optional<String> maybeRecordLabel() const = 0;
    virtual Array<String> tags() const = 0;
    virtual Array<String> artists() const = 0;
    virtual Array<String> producers() const = 0;
    virtual Array<String> remixers() const = 0;
    virtual Optional<Date> maybeDateAdded() const = 0;
    virtual Optional<Date> maybeDateReleased() const = 0;
    virtual Optional<boolean> maybeBeatGridLocked() const = 0;
    virtual Optional<count> maybeBitDepthInBits() const = 0;
    virtual Optional<count> maybeBitRateInKiloBitsPerSecond() const = 0;
    virtual Optional<DecimalNumber> maybeBeatsPerMinute() const = 0;
    virtual Optional<Color> maybeColor() const = 0;
    virtual Optional<count> maybeFileSizeInBytes() const = 0;
    virtual AudioFileType fileType() const = 0;
    virtual Array<String> musicalKeys() const = 0;
    virtual Optional<DecimalNumber> maybeLengthInSeconds() const = 0;
    virtual Optional<count> maybeTrackNumber() const = 0;
    virtual Optional<count> maybeDiscNumber() const = 0;
    virtual Optional<count> maybePlayCount() const = 0;
    virtual Optional<Common::TrackRating> maybeRating() const = 0;
    virtual String ratingAsString() const = 0;
    virtual Optional<count> maybeSampleRateInHertz() const = 0;
    virtual count numberOfMarkers() const = 0;
    virtual MarkerOfSomeSort markerAtIndex(count) const = 0;
    virtual Array<MarkerOfSomeSort> markers() const = 0;
};

class MutableTrack : public Uncopyable
{
    // -- Private Instance Methods
    void p_appendNewMarkerFrom(const Common::CueMarker& marker)
    {
        this->appendCueMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(marker);
    }
    void p_appendNewMarkerFrom(const Common::LoopMarker& marker)
    {
        this->appendLoopMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(marker);
    }
    void p_appendNewMarkerFrom(const Common::GridMarker& marker)
    {
        this->appendGridMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(marker);
    }

public:
    // -- Constructors & Destructors
    virtual ~MutableTrack() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual Volume volume() const = 0;
    virtual FilePath relativeFilePath() const = 0;
    virtual FilePath absoluteFilePath() const = 0;

    virtual Time lastModificationTime() const = 0;
    virtual void markAsModifiedNow() = 0;

    virtual Optional<String> maybeTitle() const = 0;
    virtual void setTitle(const Optional<String>&) = 0;;

    virtual void setAlbum(const Optional<String>&) = 0;
    virtual Optional<String> maybeAlbum() const = 0;

    virtual Optional<count> maybeAlbumTrackCount() const = 0;
    virtual void setAlbumTrackCount(const Optional<count>&) = 0;

    virtual Optional<String> maybeComments() const = 0;
    virtual void setComments(const Optional<String>&) = 0;

    virtual Array<String> genres() const = 0;
    virtual void setGenres(const Array<String>&) = 0;

    virtual Optional<String> maybeGrouping() const = 0;
    virtual void setGrouping(const Optional<String>&) = 0;

    virtual Optional<String> maybeMixName() const = 0;
    virtual void setMixName(const Optional<String>&) = 0;

    virtual Optional<String> maybeRecordLabel() const = 0;
    virtual void setRecordLabel(const Optional<String>&) = 0;

    virtual Array<String> tags() const = 0;
    virtual void setTags(const Array<String>&) = 0;

    virtual Array<String> artists() const = 0;
    virtual void setArtists(const Array<String>&) = 0;

    virtual Array<String> producers() const = 0;
    virtual void setProducers(const Array<String>&) = 0;

    virtual Array<String> remixers() const = 0;
    virtual void setRemixers(const Array<String>&) = 0;

    virtual Optional<Date> maybeDateAdded() const = 0;
    virtual void setDateAdded(const Optional<Date>&) = 0;

    virtual Optional<Date> maybeDateReleased() const = 0;
    virtual void setDateReleased(const Optional<Date>& maybeValue) = 0;

    virtual Optional<boolean> maybeBeatGridLocked() const = 0;
    virtual void setBeatGridLocked(boolean) = 0;

    virtual Optional<count> maybeBitDepthInBits() const = 0;
    virtual void setBitDepthInBits(const Optional<count>&) = 0;

    virtual Optional<count> maybeBitRateInKiloBitsPerSecond() const = 0;
    virtual void setBitRateInKiloBitsPerSecond(const Optional<count>&) = 0;

    virtual Optional<DecimalNumber> maybeBeatsPerMinute() const = 0;
    virtual void setBeatsPerMinute(const Optional<DecimalNumber>&) = 0;

    virtual Optional<Color> maybeColor() const = 0;
    virtual void setColor(const Optional<Color>&) = 0;

    virtual Optional<count> maybeFileSizeInBytes() const = 0;
    virtual void setFileSizeInBytes(const Optional<count>&) = 0;

    virtual AudioFileType fileType() const = 0;
    virtual void setFileType(AudioFileType) = 0;

    virtual Array<String> musicalKeys() const = 0;
    virtual void setMusicalKeys(const Array<String>&) = 0;

    virtual Optional<DecimalNumber> maybeLengthInSeconds() const = 0;
    virtual void setLengthInSeconds(const Optional<DecimalNumber>&) = 0;

    virtual Optional<count> maybeTrackNumber() const = 0;
    virtual void setTrackNumber(const Optional<count>&) = 0;

    virtual Optional<count> maybeDiscNumber() const = 0;
    virtual void setDiscNumber(const Optional<count>&) = 0;

    virtual Optional<count> maybePlayCount() const = 0;
    virtual void setPlayCount(const Optional<count>&) = 0;

    virtual String ratingAsString() const = 0;
    virtual Optional<Common::TrackRating> maybeRating() const = 0;
    virtual void setRating(const Optional<Common::TrackRating>&) = 0;

    virtual Optional<count> maybeSampleRateInHertz() const = 0;
    virtual void setSampleRateInHertz(const Optional<count>&) = 0;

    virtual count numberOfMarkers() const = 0;
    virtual MarkerOfSomeSort markerAtIndex(count) const = 0;
    virtual MutableMarkerOfSomeSort markerAtIndex(count) = 0;
    virtual Array<MarkerOfSomeSort> markers() const = 0;
    virtual Array<MutableMarkerOfSomeSort> markers() = 0;
    virtual NotNull<MutableCueMarker*> appendCueMarker() = 0;
    virtual NotNull<MutableGridMarker*> appendGridMarker() = 0;
    virtual NotNull<MutableLoopMarker*> appendLoopMarker() = 0;
    virtual void removeMarkerAtIndex(count) = 0;
    virtual void setMarkersAndMaybeAddOffsetInSeconds(const Array<MarkerOfSomeSort>&, Optional<DecimalNumber> = nothing) = 0;

    template <class T>
        void setWithSamePropertiesAs(const T&);
};

} }
