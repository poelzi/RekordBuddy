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
#include <CommonCollection/Tracks/TrackColor.hpp>

#include <EngineCollection/Markers/Markers.hpp>
#include <EngineCollection/Crates/Playlist.hpp>

#include <TrackFiles/TrackFile.hpp>

#include <Base/Array.hpp>
#include <Base/Assert.hpp>
#include <Base/FilePath.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>

namespace NxA { namespace Engine {

// -- Forward Declarations
class MutableCollection;

// -- Internal Interface
class MutableTrack : public Common::MutableTrack, public Common::Track
{
    // -- Friends
    friend class MutableCollection;

    // -- Private Instance Variables
    Pointer<MutableCollection> p_collection;

    mutable Optional<MutableArray<Shared<MutableMarkerOfSomeSort>>> p_maybeMarkers;

    Time p_lastModificationTime;

    boolean p_markersMayBeModified = false;

    Optional<count> p_maybeTrackID;
    mutable Optional<FilePath> p_maybeLocation;

    // -- Private Instance Methods
    MutableArray<Shared<MutableMarkerOfSomeSort>>& p_ensureMarkersAreLoaded() const;

    inline void p_appendCommonMarkerAndMaybeAddOffsetInSeconds(const NotNull<const Common::CueMarker*>& marker,
                                                               Optional<DecimalNumber> maybeOffset)
    {
        this->appendCueMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(*marker, maybeOffset);
    }
    inline void p_appendCommonMarkerAndMaybeAddOffsetInSeconds(const NotNull<const Common::GridMarker*>& marker,
                                                               Optional<DecimalNumber> maybeOffset)
    {
        this->appendGridMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(*marker, maybeOffset);
    }
    inline void p_appendCommonMarkerAndMaybeAddOffsetInSeconds(const NotNull<const Common::LoopMarker*>& marker,
                                                               Optional<DecimalNumber> maybeOffset)
    {
        this->appendLoopMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(*marker, maybeOffset);
    }

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

public:
    // -- Constructors & Destructors
    MutableTrack(Pointer<MutableCollection>, const Protected&);
    ~MutableTrack() override = default;

    // -- Instance Methods
    NotNull<const Common::Collection*> collection() const override;
    NotNull<Common::MutableCollection*> collection() override;

    Volume volume() const override
    {
        return Volume{ this->absoluteFilePath() };
    }
    FilePath relativeFilePath() const override
    {
        auto filePath = this->absoluteFilePath();
        auto maybeRelativePath = filePath.maybeRelativeToVolume();
        NXA_ASSERT_TRUE_WITH_BLOCK(maybeRelativePath.isValid(), [&filePath]() {
            CrashLog::addUserInfoWithKey(filePath.asEncodedString(), "filepath");
            CrashLog::addUserInfoWithKey(Volume{ filePath }.asFilePath().asEncodedString(), "volume");
        });

        return *maybeRelativePath;
    }
    FilePath absoluteFilePath() const override
    {
        NXA_ALOG("To be implemented.");
    }

    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
    }
    void markAsModifiedNow() override;

    Optional<String> maybeTitle() const override
    {
        return nothing;
    }
    void setTitle(const Optional<String>& maybeValue) override
    {
    }

    Optional<String> maybeAlbum() const override
    {
        return nothing;
    }
    void setAlbum(const Optional<String>& maybeValue) override
    {
    }

    Optional<count> maybeAlbumTrackCount() const override
    {
        return nothing;
    }
    void setAlbumTrackCount(const Optional<count>& maybeValue) override
    {
    }

    Optional<String> maybeComments() const override
    {
        return nothing;
    }
    void setComments(const Optional<String>& maybeValue) override
    {
    }

    Array<String> genres() const override;
    void setGenres(const Array<String>& values) override;

    Optional<String> maybeGrouping() const override
    {
        return nothing;
    }
    void setGrouping(const Optional<String>& maybeValue) override
    {
    }

    Optional<String> maybeMixName() const override
    {
        return nothing;
    }
    void setMixName(const Optional<String>& maybeValue) override
    {
    }

    Optional<String> maybeRecordLabel() const override
    {
        return nothing;
    }
    void setRecordLabel(const Optional<String>& maybeValue) override
    {
    }

    Array<String> tags() const override
    {
        return { };
    }
    void setTags(const Array<String>& values) override
    {
    }

    Array<String> artists() const override;
    void setArtists(const Array<String>&) override;

    Array<String> producers() const override;
    void setProducers(const Array<String>&) override;

    Array<String> remixers() const override;
    void setRemixers(const Array<String>&) override;

    Optional<Date> maybeDateAdded() const override
    {
        return nothing;
    }
    void setDateAdded(const Optional<Date>& maybeValue) override
    {
    }

    Optional<Date> maybeDateReleased() const override
    {
        return nothing;
    }
    void setDateReleased(const Optional<Date>& maybeValue) override
    {
    }

    Optional<boolean> maybeBeatGridLocked() const override
    {
        return nothing;
    }
    void setBeatGridLocked(boolean value) override
    {
    }

    Optional<count> maybeBitDepthInBits() const override
    {
        return nothing;
    }
    void setBitDepthInBits(const Optional<count>& maybeValue) override
    {
    }

    Optional<count> maybeBitRateInKiloBitsPerSecond() const override
    {
        return nothing;
    }
    void setBitRateInKiloBitsPerSecond(const Optional<count>& maybeValue) override
    {
    }

    Optional<DecimalNumber> maybeBeatsPerMinute() const override
    {
        return nothing;
    }
    void setBeatsPerMinute(const Optional<DecimalNumber>& maybeValue) override
    {
    }

    Optional<Color> maybeColor() const override
    {
        return nothing;
    }
    void setColor(const Optional<Color>& maybeValue) override
    {
    }

    Optional<count> maybeFileSizeInBytes() const override
    {
        return nothing;
    }
    void setFileSizeInBytes(const Optional<count>& maybeValue) override
    {
    }

    AudioFileType fileType() const override;
    void setFileType(AudioFileType) override;

    Array<String> musicalKeys() const override
    {
        return { };
    }
    void setMusicalKeys(const Array<String>& values) override
    {
    }

    Optional<DecimalNumber> maybeLengthInSeconds() const override
    {
        return nothing;
    }
    void setLengthInSeconds(const Optional<DecimalNumber>& maybeValue) override
    {
    }

    Optional<count> maybeTrackNumber() const override
    {
        return nothing;
    }
    void setTrackNumber(const Optional<count>& maybeValue) override
    {
    }

    Optional<count> maybeDiscNumber() const override
    {
        return nothing;
    }
    void setDiscNumber(const Optional<count>& maybeValue) override
    {
    }

    Optional<count> maybePlayCount() const override
    {
        return nothing;
    }
    void setPlayCount(const Optional<count>& maybeValue) override
    {
    }

    String ratingAsString() const override
    {
        return { };
    }
    Optional<Common::TrackRating> maybeRating() const override
    {
        return nothing;
    }
    void setRating(const Optional<Common::TrackRating>& maybeValue) override
    {
    }

    Optional<count> maybeSampleRateInHertz() const override
    {
        return nothing;
    }
    void setSampleRateInHertz(const Optional<count>& maybeValue) override
    {
    }

    count numberOfMarkers() const override
    {
        return 0;
    }
    Common::MarkerOfSomeSort markerAtIndex(count index) const override
    {
        NXA_ALOG("No markers yet.");
    }
    Common::MutableMarkerOfSomeSort markerAtIndex(count index) override
    {
        NXA_ALOG("No markers yet.");
    }
    Array<Common::MarkerOfSomeSort> markers() const override
    {
        return { };
    }
    Array<Common::MutableMarkerOfSomeSort> markers() override
    {
        return { };
    }
    NotNull<Common::MutableCueMarker *> appendCueMarker() override
    {
        NXA_ALOG("Can't add markers yet.");
    }
    NotNull<Common::MutableGridMarker *> appendGridMarker() override
    {
        NXA_ALOG("Can't add markers yet.");
    }
    NotNull<Common::MutableLoopMarker *> appendLoopMarker() override
    {
        NXA_ALOG("Can't add markers yet.");
    }
    void removeMarkerAtIndex(count index) override
    {
    }
    void setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>&, Optional<DecimalNumber> = nothing) override;

    Optional<count> maybeTrackID() const
    {
        return nothing;
    }
    void setTrackID(count trackID)
    {
    }
};

} }
