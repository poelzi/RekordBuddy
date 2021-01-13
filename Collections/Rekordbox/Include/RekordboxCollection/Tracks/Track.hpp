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

#include <RekordboxCollection/Markers/Markers.hpp>
#include <RekordboxCollection/Crates/Playlist.hpp>

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

namespace NxA { namespace Rekordbox {

// -- Forward Declarations
class MutableCollection;

// -- Internal Interface
class MutableTrack : public Common::MutableTrack, public Common::Track
{
    // -- Friends
    friend class MutableCollection;

#if defined(NXA_BUILD_FOR_TESTING)
    friend class RekordboxTrackTests;
#endif

    // -- Private Instance Variables
    Pointer<MutableCollection> p_collection;

    mutable MutableXMLNode p_rekordboxTrack;
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
            auto maybeLocation = this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Location");
            NXA_ASSERT_TRUE(maybeLocation.isValid());

            auto location = maybeLocation->asStringByRemovingPercentEncoding();
            NXA_ASSERT_TRUE(location.hasPrefix("file://localhost/"));
#if defined(NXA_PLATFORM_WINDOWS)
            location = location.subString(17);
            NXA_ASSERT_TRUE_WITH_BLOCK((location.length() > 2) && (location[1] == ':'), [this]() {
                CrashLog::addUserInfoWithKey(this->p_rekordboxTrack.asString(), "trackNode");
            });
#elif defined(NXA_PLATFORM_MACOS)
            location = location.subString(16);
            if (location.hasPrefix("//")) {
                // -- We have files with incorrect paths like this so we correct them here.
                location = location.subString(1);

                auto mutableThis = const_cast<MutableTrack*>(this);
                mutableThis->p_rekordboxTrack.setStringValueForAttributeNamed(maybeLocation->stringByReplacingOccurencesOfWith("file://localhost//",
                                                                                                                               "file://localhost/"),
                                                                              "Location");
                mutableThis->markAsModifiedNow();
            }
#else
            #error Unsupported platform.
#endif

            NXA_ASSERT_FALSE(location.isEmpty());

            this->p_maybeLocation = { FilePath{ location } };
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
        return this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Name");
    }
    void setTitle(const Optional<String>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue, "Name");
    }

    Optional<String> maybeAlbum() const override
    {
        return this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Album");
    }
    void setAlbum(const Optional<String>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue, "Album");
    }

    Optional<count> maybeAlbumTrackCount() const override
    {
        // -- This is not supported by rekordbox.
        return nothing;
    }
    void setAlbumTrackCount(const Optional<count>& maybeValue) override
    {
        // -- This is not supported by rekordbox.
    }

    Optional<String> maybeComments() const override
    {
        return this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Comments");
    }
    void setComments(const Optional<String>& maybeValue) override
    {
        // -- rekordbox seems to have a bug when exporting track data to USB if the Comments field is too long.
        // -- for now we will just limit it to 1000 characters on export.
        return this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue.isValid() ?
                                                                      ((maybeValue->length() > 1000) ? maybeValue->subString(0, 1000) : *maybeValue) :
                                                                      Optional<String>{ },
                                                                      "Comments");
    }

    Array<String> genres() const override;
    void setGenres(const Array<String>& values) override;

    Optional<String> maybeGrouping() const override
    {
        return this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Grouping");
    }
    void setGrouping(const Optional<String>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue, "Grouping");
    }

    Optional<String> maybeMixName() const override
    {
        return this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Mix");
    }
    void setMixName(const Optional<String>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue, "Mix");
    }

    Optional<String> maybeRecordLabel() const override
    {
        return this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Label");
    }
    void setRecordLabel(const Optional<String>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue, "Label");
    }

    Array<String> tags() const override
    {
        // -- This is not supported by rekordbox.
        return { };
    }
    void setTags(const Array<String>& values) override
    {
        // -- This is supported by rekordbox but not currently exposed in the XML.
    }

    Array<String> artists() const override;
    void setArtists(const Array<String>&) override;

    Array<String> producers() const override;
    void setProducers(const Array<String>&) override;

    Array<String> remixers() const override;
    void setRemixers(const Array<String>&) override;

    Optional<Date> maybeDateAdded() const override
    {
        return this->p_rekordboxTrack.maybeDateValueForAttributeNamedAndDateSeparator("DateAdded", '-');
    }
    void setDateAdded(const Optional<Date>& maybeValue) override
    {
        this->p_rekordboxTrack.setDateValueForAttributeNamedAndDateSeparator(maybeValue, "DateAdded", '-');
    }

    Optional<Date> maybeDateReleased() const override
    {
        auto maybeYear = this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Year");
        if (!maybeYear.isValid() || (maybeYear->length() != 4)) {
            return nothing;
        }

        return Date::maybeDateWithYear(maybeYear->integerValue());
    }
    void setDateReleased(const Optional<Date>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue.isValid() ?
                                                               maybeValue->asStringWithJustYear() :
                                                               Optional<String>{ }, "Year");
    }

    Optional<boolean> maybeBeatGridLocked() const override
    {
        // -- This is supported by rekordbox but not currently exposed in the XML.
        return nothing;
    }
    void setBeatGridLocked(boolean value) override
    {
        // -- This is supported by rekordbox but not currently exposed in the XML.
    }

    Optional<count> maybeBitDepthInBits() const override
    {
        // -- This is not supported by rekordbox.
        return nothing;
    }
    void setBitDepthInBits(const Optional<count>& maybeValue) override
    {
        // -- This is supported by rekordbox but not currently exposed in the XML.
    }

    Optional<count> maybeBitRateInKiloBitsPerSecond() const override
    {
        return this->p_rekordboxTrack.maybeCountValueForAttributeNamed("BitRate");
    }
    void setBitRateInKiloBitsPerSecond(const Optional<count>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue, "BitRate");
    }

    Optional<DecimalNumber> maybeBeatsPerMinute() const override
    {
        auto maybeAverageBpm = this->p_rekordboxTrack.maybeStringValueForAttributeNamed("AverageBpm");
        if (!maybeAverageBpm.isValid()) {
            return nothing;
        }

        return DecimalNumber{ *maybeAverageBpm };
    }
    void setBeatsPerMinute(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_rekordboxTrack.setStringValueForAttributeNamed(maybeValue.isValid() ?
                                                               maybeValue->asStringWithFractionDigitsBetween(0,2) :
                                                               Optional<String>{ },
                                                               "AverageBpm");
    }

    Optional<Color> maybeColor() const override
    {
        auto maybeColor = this->p_rekordboxTrack.maybeHexadecimalValueForAttributeNamed("Colour");
        if (!maybeColor.isValid() || (*maybeColor > 0xffffff)) {
            return nothing;
        }

        // -- rekordbox color values are just rgb, with no alpha, so we shift them up and apply the full alpha.
        return Color{ (static_cast<uinteger32>(*maybeColor) << 8) | 0xff };
    }
    void setColor(const Optional<Color>& maybeValue) override
    {
        Optional<count> maybeColorAsInteger;
        if (maybeValue.isValid()) {
            auto maybeColor = Common::TrackColor::maybeColorForTrackColorWhenExportedTo(*maybeValue, Common::Collection::Type::rekordbox);
            if (maybeColor.isValid()) {
                maybeColorAsInteger = maybeColor->asRGB();
            }
        }

        this->p_rekordboxTrack.setCountValueWithFormatForAttributeNamed(maybeColorAsInteger, "0x%06x", "Colour");
    }

    Optional<count> maybeFileSizeInBytes() const override
    {
        return this->p_rekordboxTrack.maybeCountValueForAttributeNamed("Size");
    }
    void setFileSizeInBytes(const Optional<count>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue, "Size");
    }

    AudioFileType fileType() const override;
    void setFileType(AudioFileType) override;

    Array<String> musicalKeys() const override
    {
        auto maybeMusicalKey = this->p_rekordboxTrack.maybeStringValueForAttributeNamed("Tonality");
        if (maybeMusicalKey.isValid() && maybeMusicalKey->length() != 0) {
            return { *maybeMusicalKey };
        }

        return { };
    }
    void setMusicalKeys(const Array<String>& values) override
    {
        if (values.length() > 0) {
            auto maybeConvertedValue = Common::MusicalKey::maybeStringValueInDefaultNotationFromString(values.firstObject());
            if (maybeConvertedValue.isValid()) {
                this->p_rekordboxTrack.setStringValueForAttributeNamed(*maybeConvertedValue, "Tonality");
                return;
            }
        }

        this->p_rekordboxTrack.setStringValueForAttributeNamed({ }, "Tonality");
    }

    Optional<DecimalNumber> maybeLengthInSeconds() const override
    {
        auto maybeLengthInSeconds = this->p_rekordboxTrack.maybeCountValueForAttributeNamed("TotalTime");
        if (!maybeLengthInSeconds.isValid()) {
            return nothing;
        }

        return DecimalNumber::withInteger(static_cast<integer64>(*maybeLengthInSeconds));
    }
    void setLengthInSeconds(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue.isValid() ?
                                                              Optional<count>{ maybeValue->asInteger() } :
                                                              nothing,
                                                              "TotalTime");
    }

    Optional<count> maybeTrackNumber() const override
    {
        return this->p_rekordboxTrack.maybeCountValueForAttributeNamed("TrackNumber");
    }
    void setTrackNumber(const Optional<count>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue, "TrackNumber");
    }

    Optional<count> maybeDiscNumber() const override
    {
        return this->p_rekordboxTrack.maybeCountValueForAttributeNamed("DiscNumber");
    }
    void setDiscNumber(const Optional<count>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue, "DiscNumber");
    }

    Optional<count> maybePlayCount() const override
    {
        return this->p_rekordboxTrack.maybeCountValueForAttributeNamed("PlayCount");
    }
    void setPlayCount(const Optional<count>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue, "PlayCount");
    }

    String ratingAsString() const override
    {
        return Common::Track::ratingAsStringFor(*this);
    }
    Optional<Common::TrackRating> maybeRating() const override
    {
        auto maybeRating = this->p_rekordboxTrack.maybeCountValueForAttributeNamed("Rating");
        if (!maybeRating.isValid()) {
            return nothing;
        }

        return Common::TrackRating::maybeWithValue(*maybeRating);
    }
    void setRating(const Optional<Common::TrackRating>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue.isValid() ?
                                                              maybeValue->value() :
                                                              Optional<count>{ }, "Rating");
    }

    Optional<count> maybeSampleRateInHertz() const override
    {
        return this->p_rekordboxTrack.maybeCountValueForAttributeNamed("SampleRate");
    }
    void setSampleRateInHertz(const Optional<count>& maybeValue) override
    {
        this->p_rekordboxTrack.setCountValueForAttributeNamed(maybeValue, "SampleRate");
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
        this->p_rekordboxTrack.setCountValueForAttributeNamed(trackID, "TrackID");
    }
};

} }
