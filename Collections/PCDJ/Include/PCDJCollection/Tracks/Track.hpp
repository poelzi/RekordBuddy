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

#include <PCDJCollection/Markers/Markers.hpp>
#include <PCDJCollection/Crates/Playlist.hpp>

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

namespace NxA { namespace PCDJ {

// -- Forward Declarations
class MutableCollection;

// -- Internal Interface
class MutableTrack : public Common::MutableTrack, public Common::Track
{
    // -- Friends
    friend class MutableCollection;

#if defined(NXA_BUILD_FOR_TESTING)
    friend class PCDJTrackTests;
#endif

    // -- Private Instance Variables
    Pointer<MutableCollection> p_collection;

    mutable MutableXMLNode p_pcdjTrack;
    mutable Optional<MutableArray<Shared<MutableMarkerOfSomeSort>>> p_maybeMarkers;

    Time p_lastModificationTime;

    boolean p_markersMayBeModified = false;

    Optional<count> p_maybeTrackID;
    mutable Optional<FilePath> p_maybeLocation;

    // -- Private Instance Methods
    MutableArray<Shared<MutableMarkerOfSomeSort>>& p_ensureMarkersAreLoaded() const;

    NXA_VIRTUAL_FOR_TESTING void p_updateMarkersInXML();

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

    // -- Protected Instance Variables
#if defined(NXA_BUILD_FOR_TESTING)
    void p_testClearMarkerCache()
    {
        this->p_maybeMarkers = nothing;
    }
#endif

public:
    // -- Factory Methods
    static Optional<Shared<MutableTrack>> maybeTrackWithNodeInCollection(MutableXMLNode, Pointer<MutableCollection>, const Protected&);

    // -- Constructors & Destructors
    MutableTrack(MutableXMLNode, Pointer<MutableCollection>, const Protected&);
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
        if (!this->p_maybeLocation.isValid()) {
            auto maybeLocation = this->p_pcdjTrack.maybeStringValueForAttributeNamed("fnam");
            NXA_ASSERT_TRUE(maybeLocation.isValid());

            this->p_maybeLocation = { FilePath{ *maybeLocation } };
        }

        return *this->p_maybeLocation;
    }

    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
    }
    void markAsModifiedNow() override;

    Optional<String> maybeTitle() const override
    {
        return this->p_pcdjTrack.maybeStringValueForAttributeNamed("titl");
    }
    void setTitle(const Optional<String>& maybeValue) override
    {
        this->p_pcdjTrack.setStringValueForAttributeNamed(maybeValue, "titl");
    }

    Optional<String> maybeAlbum() const override
    {
        return this->p_pcdjTrack.maybeStringValueForAttributeNamed("albm");
    }
    void setAlbum(const Optional<String>& maybeValue) override
    {
        this->p_pcdjTrack.setStringValueForAttributeNamed(maybeValue, "albm");
    }

    Optional<count> maybeAlbumTrackCount() const override
    {
        // -- This is not supported by PCDJ.
        return nothing;
    }
    void setAlbumTrackCount(const Optional<count>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<String> maybeComments() const override
    {
        return this->p_pcdjTrack.maybeStringValueForAttributeNamed("comm");
    }
    void setComments(const Optional<String>& maybeValue) override
    {
        return this->p_pcdjTrack.setStringValueForAttributeNamed(*maybeValue , "comm");
    }

    Array<String> genres() const override;
    void setGenres(const Array<String>& values) override;

    Optional<String> maybeGrouping() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setGrouping(const Optional<String>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<String> maybeMixName() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setMixName(const Optional<String>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<String> maybeRecordLabel() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setRecordLabel(const Optional<String>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Array<String> tags() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setTags(const Array<String>& values) override
    {
        // -- This is not supported by PCDJ.
    }

    Array<String> artists() const override;
    void setArtists(const Array<String>&) override;

    virtual Array<String> producers() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setProducers(const Array<String>&) override
    {
        // -- This is not supported by PCDJ.
    }

    virtual Array<String> remixers() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    virtual void setRemixers(const Array<String>&) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<Date> maybeDateAdded() const override
    {
        auto maybeDateAdded = this->p_pcdjTrack.maybeCountValueForAttributeNamed("dtAdded");
        if (!maybeDateAdded.isValid() || (*maybeDateAdded == 0)) {
            return nothing;
        }

        auto maybeNarrowed = maybeNarrowCast<timestamp>(*maybeDateAdded);
        if (!maybeNarrowed.isValid()) {
            return nothing;
        }

        return Date::inLocalTimeZoneFromTime(Time{ *maybeNarrowed });
    }
    void setDateAdded(const Optional<Date>& maybeValue) override
    {
        timestamp dateAsTimeStamp = 0;

        if (maybeValue.isValid()) {
            dateAsTimeStamp = maybeValue->asTimeConvertedFromLocalTimeZone().asUnixTimeStamp();
        }

        this->p_pcdjTrack.setCountValueForAttributeNamed(dateAsTimeStamp, "dtAdded");
    }

    Optional<Date> maybeDateReleased() const override
    {
        auto maybeYear = this->p_pcdjTrack.maybeStringValueForAttributeNamed("year");
        if (!maybeYear.isValid() || (maybeYear->length() != 4)) {
            return nothing;
        }

        return Date::maybeDateWithYear(maybeYear->integerValue());
    }
    void setDateReleased(const Optional<Date>& maybeValue) override
    {
        this->p_pcdjTrack.setStringValueForAttributeNamed(maybeValue.isValid() ?
                                                               maybeValue->asStringWithJustYear() :
                                                               Optional<String>{ }, "year");
    }

    Optional<boolean> maybeBeatGridLocked() const override
    {
        // -- This is not supported by PCDJ.
        return nothing;
    }
    void setBeatGridLocked(boolean value) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<count> maybeBitDepthInBits() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("bitDepth");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return *maybeValue;
    }
    void setBitDepthInBits(const Optional<count>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue, "bitDepth");
    }

    Optional<count> maybeBitRateInKiloBitsPerSecond() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("bitRate");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return { *maybeValue / 1000 };
    }
    void setBitRateInKiloBitsPerSecond(const Optional<count>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue, "bitRate");
    }

    Optional<DecimalNumber> maybeBeatsPerMinute() const override
    {
        auto maybeAverageBpm = this->p_pcdjTrack.maybeDecimalValueForAttributeNamed("bpm");
        if (!maybeAverageBpm.isValid() || maybeAverageBpm->isZero()) {
            return nothing;
        }

        return DecimalNumber{ *maybeAverageBpm };
    }
    void setBeatsPerMinute(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_pcdjTrack.setDecimalValueWithFractionDigitsForAttributeNamed(maybeValue, 4, "bpm");
    }

    Optional<Color> maybeColor() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setColor(const Optional<Color>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<count> maybeFileSizeInBytes() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("size");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return *maybeValue;
    }
    void setFileSizeInBytes(const Optional<count>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue, "size");
    }

    AudioFileType fileType() const override
    {
        // -- 0 audio; 1 video; 2 karaoke;
        auto maybeType = this->p_pcdjTrack.maybeCountValueForAttributeNamed("mediatype");
        if (maybeType.isValid()) {
            if (*maybeType == 0) {
                return genericTypeForAudioFileAt(this->absoluteFilePath());
            }
            else if (*maybeType == 1) {
                return AudioFileType::Movie;
            }
        }

        return AudioFileType::Unknown;
    }
    void setFileType(AudioFileType type) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed( (type == AudioFileType::Movie) ? 1 : 0, "mediatype");
    }

    Array<String> musicalKeys() const override;
    void setMusicalKeys(const Array<String>& values) override;

    Optional<DecimalNumber> maybeLengthInSeconds() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("duration_ms");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return DecimalNumber::withInteger(static_cast<integer64>(*maybeValue / 1000));
    }
    void setLengthInSeconds(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue.isValid() ?
                                                         Optional<count>{ maybeValue->asInteger() * 1000 } :
                                                         nothing,
                                                         "duration_ms");
    }

    Optional<count> maybeTrackNumber() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("trck");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return maybeValue;
    }
    void setTrackNumber(const Optional<count>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue, "trck");
    }

    Optional<count> maybeDiscNumber() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setDiscNumber(const Optional<count>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<count> maybePlayCount() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("playCount");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return maybeValue;
    }
    void setPlayCount(const Optional<count>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue, "playCount");
    }

    String ratingAsString() const override
    {
        return Common::Track::ratingAsStringFor(*this);
    }
    Optional<Common::TrackRating> maybeRating() const override
    {
        // -- This is not supported by PCDJ.
        return { };
    }
    void setRating(const Optional<Common::TrackRating>& maybeValue) override
    {
        // -- This is not supported by PCDJ.
    }

    Optional<count> maybeSampleRateInHertz() const override
    {
        auto maybeValue = this->p_pcdjTrack.maybeCountValueForAttributeNamed("sr");
        if (!maybeValue.isValid() || (*maybeValue == 0)) {
            return nothing;
        }

        return maybeValue;
    }
    void setSampleRateInHertz(const Optional<count>& maybeValue) override
    {
        this->p_pcdjTrack.setCountValueForAttributeNamed(maybeValue, "sr");
    }

    count numberOfMarkers() const override
    {
        return this->p_ensureMarkersAreLoaded().length();
    }
    Common::MarkerOfSomeSort markerAtIndex(count index) const override
    {
        return withVariant(*this->p_ensureMarkersAreLoaded()[index], [](auto& marker) {
            return Common::MarkerOfSomeSort{ &marker };
        });
    }
    Common::MutableMarkerOfSomeSort markerAtIndex(count index) override
    {
        this->p_markersMayBeModified = true;

        return withVariant(*this->p_ensureMarkersAreLoaded()[index], [](auto& marker) {
            return Common::MutableMarkerOfSomeSort{ &marker };
        });
    }
    Array<Common::MarkerOfSomeSort> markers() const override
    {
        MutableArray<Common::MarkerOfSomeSort> results;

        for (auto&& marker : this->p_ensureMarkersAreLoaded()) {
            withVariant(*marker, [&results](auto& marker) {
                results.emplaceAppend(&marker.asImmutableReference());
            });
        }

        return std::move(results);
    }
    Array<Common::MutableMarkerOfSomeSort> markers() override
    {
        this->p_markersMayBeModified = true;

        MutableArray<Common::MutableMarkerOfSomeSort> results;

        for (auto&& marker : this->p_ensureMarkersAreLoaded()) {
            withVariant(*marker, [&results](auto& marker) {
                results.emplaceAppend(&marker);
            });
        }

        return std::move(results);
    }
    NotNull<Common::MutableCueMarker *> appendCueMarker() override
    {
        this->p_markersMayBeModified = true;

        auto& markers = this->p_ensureMarkersAreLoaded();
        markers.append(Shared<MutableMarkerOfSomeSort>::with(MutableCueMarker{ DecimalNumber{ }, MutableCueMarker::p_isProtected }));

        return &markers.lastObject()->get<MutableCueMarker>();
    }
    NotNull<Common::MutableGridMarker *> appendGridMarker() override
    {
        this->p_markersMayBeModified = true;

        auto& markers = this->p_ensureMarkersAreLoaded();
        markers.append(Shared<MutableMarkerOfSomeSort>::with(MutableGridMarker{ DecimalNumber{ },
                                                                                DecimalNumber::withInteger(123),
                                                                                MutableGridMarker::p_isProtected }));

        return &markers.lastObject()->get<MutableGridMarker>();
    }
    NotNull<Common::MutableLoopMarker *> appendLoopMarker() override
    {
        this->p_markersMayBeModified = true;

        auto& markers = this->p_ensureMarkersAreLoaded();
        markers.append(Shared<MutableMarkerOfSomeSort>::with(MutableLoopMarker{ DecimalNumber{ },
                                                                                DecimalNumber::withInteger(1),
                                                                                MutableLoopMarker::p_isProtected }));

        return &markers.lastObject()->get<MutableLoopMarker>();
    }
    void removeMarkerAtIndex(count index) override
    {
        this->p_markersMayBeModified = true;

        this->p_ensureMarkersAreLoaded().removeObjectAtIndex(index);
    }
    void setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>&, Optional<DecimalNumber> = nothing) override;

    NXA_VIRTUAL_FOR_TESTING Optional<count> maybeTrackID() const
    {
        return this->p_maybeTrackID;
    }
    void setTrackID(count trackID)
    {
        this->p_maybeTrackID = trackID;
        this->p_pcdjTrack.setCountValueForAttributeNamed(trackID, "id");
    }
};

} }
