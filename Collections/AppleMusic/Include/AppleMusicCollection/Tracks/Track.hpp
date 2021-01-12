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

#include <CommonCollection/Tracks/Track.hpp>

#include <AppleMusicCollection/Crates/Playlist.hpp>

#include <CommonCollection/Tracks/TrackColor.hpp>

#include <TrackFiles/TrackFile.hpp>

#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/FilePath.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/XMLNode.hpp>

#include <Base/TestUtility.hpp>

namespace NxA { namespace AppleMusic {

// -- Forward Declarations
class Collection;

// -- Internal Interface
class Track : public Common::Track
{
    // -- Friends
    friend class Collection;

#if defined(NXA_BUILD_FOR_TESTING)
    friend class ITunesTrackTests;
#endif

    // -- Private Instance Variables
    Pointer<Collection> p_collection;

    mutable XMLNode p_appleMusicTrack;

    mutable Optional<DecimalNumber> p_maybeLengthInSeconds;
    mutable Optional<count> p_maybeFileSizeInBytes;
    mutable Optional<count>p_maybeTrackNumber;
    mutable Optional<Date> p_maybeDateReleased;
    mutable Optional<DecimalNumber> p_maybeBeatsPerMinute;
    mutable Optional<Date> p_maybeDateAdded;
    mutable Optional<count> p_maybeBitRateInKiloBitsPerSecond;
    mutable Optional<count> p_maybeSampleRateInHertz;
    mutable Optional<String> p_maybeTitle;
    mutable Array<String> p_artists;
    mutable Optional<String> p_maybeAlbum;
    mutable Array<String> p_genres;
    mutable Optional<String> p_maybeComments;
    mutable Optional<String> p_maybeRecordLabel;

    mutable AudioFileType p_fileType = AudioFileType::Unknown;
    mutable Time p_lastModificationTime = Time::distantPast();
    mutable FilePath p_absoluteFilePath;

    mutable boolean p_trackInfoLoaded = false;

    // -- Private Instance Methods
    void p_ensureTrackDataIsLoaded() const;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

public:
    // -- Factory Methods
    static Optional<Shared<Track>> maybeTrackWithNodeInCollection(XMLNode track, Pointer<Collection> inCollection, const Protected&);

    // -- Constructors & Destructors
    Track(XMLNode, const FilePath&, Pointer<Collection>, const Protected&);
    ~Track() override = default;

    // -- Instance Methods
    NotNull<const Common::Collection*> collection() const override;

    Volume volume() const override
    {
        return Volume{ this->absoluteFilePath() };
    }
    FilePath relativeFilePath() const override
    {
        auto absoluteFilePath = this->absoluteFilePath();
        auto maybeRelativePath = absoluteFilePath.maybeRelativeToVolume();
        NXA_ASSERT_TRUE_WITH_BLOCK(maybeRelativePath.isValid(), [&absoluteFilePath]() {
            CrashLog::addUserInfoWithKey(absoluteFilePath.asEncodedString(), "filepath");
            CrashLog::addUserInfoWithKey(Volume{ absoluteFilePath }.asFilePath().asEncodedString(), "volume");
        });

        return *maybeRelativePath;
    }
    FilePath absoluteFilePath() const override
    {
        return this->p_absoluteFilePath;
    }

    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
    }

    Optional<String> maybeTitle() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeTitle;
    }

    Optional<String> maybeAlbum() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeAlbum;
    }

    Optional<count> maybeAlbumTrackCount() const override
    {
        // -- This is not supported by Apple Music.
        return nothing;
    }

    Optional<String> maybeComments() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeComments;
    }

    Array<String> genres() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_genres;
    }

    Optional<String> maybeGrouping() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<String> maybeMixName() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<String> maybeRecordLabel() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeRecordLabel;
    }

    Array<String> tags() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Array<String> artists() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_artists;
    }

    Array<String> producers() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Array<String> remixers() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<Date> maybeDateAdded() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeDateAdded;
    }

    Optional<Date> maybeDateReleased() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeDateReleased;
    }

    Optional<boolean> maybeBeatGridLocked() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<count> maybeBitDepthInBits() const override
    {
        // -- This is not supported by Apple Music.
        return nothing;
    }

    Optional<count> maybeBitRateInKiloBitsPerSecond() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeBitRateInKiloBitsPerSecond;
    }

    Optional<DecimalNumber> maybeBeatsPerMinute() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeBeatsPerMinute;
    }

    Optional<Color> maybeColor() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<count> maybeFileSizeInBytes() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeFileSizeInBytes;
    }

    AudioFileType fileType() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_fileType;
    }

    Array<String> musicalKeys() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<DecimalNumber> maybeLengthInSeconds() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeLengthInSeconds;
    }

    Optional<count> maybeTrackNumber() const override
    {
        this->p_ensureTrackDataIsLoaded();
        return this->p_maybeTrackNumber;
    }

    Optional<count> maybeDiscNumber() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<count> maybePlayCount() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    String ratingAsString() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }
    Optional<Common::TrackRating> maybeRating() const override
    {
        // -- This is not supported by Apple Music.
        return { };
    }

    Optional<count> maybeSampleRateInHertz() const override
    {
        return this->p_appleMusicTrack.maybeCountValueForAttributeNamed("SampleRate");
    }

    count numberOfMarkers() const override
    {
        return 0;
    }
    Common::MarkerOfSomeSort markerAtIndex(count index) const override
    {
        NXA_ALOG("This type of tracks do not have markers.");
    }
    Array<Common::MarkerOfSomeSort> markers() const override
    {
        return { };
    }
};

} }
