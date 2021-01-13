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

#include <TraktorCollection/Markers/Markers.hpp>
#include <TraktorCollection/Crates/Playlist.hpp>

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

namespace NxA { namespace Traktor {

// -- Forward Declarations
class MutableCollection;

// -- Public Interface
class MutableTrack : public Common::MutableTrack, public Common::Track
{
    friend MutableCollection;

    // -- Friends
#if defined(NXA_BUILD_FOR_TESTING)
    friend class TraktorTrackTests;
#endif

    // -- Private Class Methods
    static FilePath p_traktorPathToAbsoluteFilePath(const String&);

    static Optional<Color> p_trackRGBColorForTraktorTrackColorIndex(count);

    static Optional<count> p_traktorTrackColorIndexForTrackRGBColor(const Color&);

    static void p_setModificationTimeOnNode(const Time&, MutableXMLNode&);

    // -- Private Instance Variables
    Pointer<MutableCollection> p_collection;

    mutable MutableXMLNode p_traktorTrack;
    mutable Optional<MutableArray<Shared<MutableMarkerOfSomeSort>>> p_maybeMarkers;
    mutable Optional<FilePath> p_maybeLocation;

    Time p_lastModificationTime;

    boolean p_markersMayBeModified = false;

    // -- Private Instance Methods
    inline void p_appendCommonMarkerAndMaybeAddOffsetInSeconds(const NotNull<const Common::CueMarker*>& marker, Optional<DecimalNumber> maybeOffset)
    {
        this->appendCueMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(*marker, maybeOffset);
    }

    inline void p_appendCommonMarkerAndMaybeAddOffsetInSeconds(const NotNull<const Common::GridMarker*>& marker, Optional<DecimalNumber> maybeOffset)
    {
        this->appendGridMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(*marker, maybeOffset);
    }

    inline void p_appendCommonMarkerAndMaybeAddOffsetInSeconds(const NotNull<const Common::LoopMarker*>& marker, Optional<DecimalNumber> maybeOffset)
    {
        this->appendLoopMarker()->setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(*marker, maybeOffset);
    }

    NXA_VIRTUAL_FOR_TESTING MutableArray<Shared<MutableMarkerOfSomeSort>>& p_ensureMarkersAreLoaded() const;

    NXA_VIRTUAL_FOR_TESTING void p_updateMarkersInXML();

    NXA_VIRTUAL_FOR_TESTING Optional<XMLNode> p_maybeAlbumNode() const;

    NXA_VIRTUAL_FOR_TESTING Optional<XMLNode> p_maybeLocationNode() const;

    NXA_VIRTUAL_FOR_TESTING Optional<XMLNode> p_maybeTempoNode() const;

    NXA_VIRTUAL_FOR_TESTING Optional<XMLNode> p_maybeMusicalKeyNode() const;

    NXA_VIRTUAL_FOR_TESTING Optional<XMLNode> p_maybeInfoNode() const;

    NXA_VIRTUAL_FOR_TESTING MutableXMLNode p_mutableInfoNode();

    NXA_VIRTUAL_FOR_TESTING MutableXMLNode p_mutableAlbumNode();

    NXA_VIRTUAL_FOR_TESTING MutableXMLNode p_mutableMusicalKeyNode();

    NXA_VIRTUAL_FOR_TESTING MutableXMLNode p_mutableTempoNode();

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
    FilePath absoluteFilePath() const override;

    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
    }

    void markAsModifiedNow() override;

    Optional<String> maybeTitle() const override
    {
        return this->p_traktorTrack.maybeNonEmptyStringValueForAttributeNamed("TITLE");
    }
    void setTitle(const Optional<String>& maybeValue) override
    {
        this->p_traktorTrack.setStringValueForAttributeNamed(maybeValue, "TITLE");
    }

    Optional<String> maybeAlbum() const override
    {
        auto maybeAlbumNode = this->p_maybeAlbumNode();
        return maybeAlbumNode.isValid() ? maybeAlbumNode->maybeNonEmptyStringValueForAttributeNamed("TITLE") : Optional<String>{ };
    }
    void setAlbum(const Optional<String>& maybeValue) override
    {
        this->p_mutableAlbumNode().setStringValueForAttributeNamed(maybeValue, "TITLE");
    }

    void setComments(const Optional<String>& maybeValue) override
    {
        this->p_mutableInfoNode().setStringValueForAttributeNamed(maybeValue, "COMMENT");
    }
    Optional<String> maybeComments() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        return maybeInfoNode.isValid() ? maybeInfoNode->maybeNonEmptyStringValueForAttributeNamed("COMMENT") : Optional<String>{ };
    }

    Array<String> genres() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        return maybeInfoNode.isValid() ? maybeInfoNode->stringValuesForAttributeNamedWhenSeparatedBy("GENRE", ", "_String) : Array<String>{ };
    }
    void setGenres(const Array<String>& values) override
    {
        this->p_mutableInfoNode().setStringValueForAttributeNamed((values.length() != 0) ? String::stringByJoiningArrayWithString(values, ", "_String) : Optional<String>{ }, "GENRE");
    }

    Optional<String> maybeGrouping() const override;
    void setGrouping(const Optional<String>& maybeValue) override;

    Optional<String> maybeMixName() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        return maybeInfoNode.isValid() ? maybeInfoNode->maybeNonEmptyStringValueForAttributeNamed("MIX") : Optional<String>{ };
    }
    void setMixName(const Optional<String>& maybeValue) override
    {
        this->p_mutableInfoNode().setStringValueForAttributeNamed(maybeValue, "MIX");
    }

    Optional<count> maybeAlbumTrackCount() const override
    {
        // -- This is not supported by Traktor.
        return nothing;
    }
    void setAlbumTrackCount(const Optional<count>& maybeValue) override
    {
        // -- This is not supported by Traktor.
    }

    Optional<String> maybeRecordLabel() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        return maybeInfoNode.isValid() ? maybeInfoNode->maybeNonEmptyStringValueForAttributeNamed("LABEL") : Optional<String>{ };
    }
    void setRecordLabel(const Optional<String>& recordLabel) override
    {
        auto maybeInfoNode = this->p_mutableInfoNode();
        maybeInfoNode.setStringValueForAttributeNamed(recordLabel, "LABEL");
    }

    Array<String> tags() const override
    {
        // -- This is not supported by Traktor.
        return { };
    }
    void setTags(const Array<String>&) override
    {
        // -- This is not supported by Traktor.
    }

    Array<String> artists() const override;
    void setArtists(const Array<String>& values) override;

    Array<String> producers() const override;
    void setProducers(const Array<String>& values) override;

    Array<String> remixers() const override;
    void setRemixers(const Array<String>& values) override;

    Optional<Date> maybeDateAdded() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        return maybeInfoNode.isValid() ? maybeInfoNode->maybeDateValueForAttributeNamedAndDateSeparator("IMPORT_DATE", '/') : Optional<Date>{ };
    }
    void setDateAdded(const Optional<Date>& maybeValue) override
    {
        this->p_mutableInfoNode().setDateValueForAttributeNamedAndDateSeparator(maybeValue, "IMPORT_DATE", '/');
    }

    Optional<Date> maybeDateReleased() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        return maybeInfoNode.isValid() ? maybeInfoNode->maybeDateValueForAttributeNamedAndDateSeparator("RELEASE_DATE", '/') : Optional<Date>{ };
    }
    void setDateReleased(const Optional<Date>& maybeValue) override
    {
        this->p_mutableInfoNode().setDateValueForAttributeNamedAndDateSeparator(maybeValue, "RELEASE_DATE", '/');
    }

    void setBeatGridLocked(boolean value) override
    {
        this->p_traktorTrack.setCountValueForAttributeNamed(value, "LOCK");
    }
    Optional<boolean> maybeBeatGridLocked() const override
    {
        auto maybeBeatGridLocked = this->p_traktorTrack.maybeCountValueForAttributeNamed("LOCK");
        if (!maybeBeatGridLocked.isValid()) {
            return nothing;
        }

        return (*maybeBeatGridLocked == 1);
    }

    Optional<count> maybeBitDepthInBits() const override
    {
        // -- This is not supported by Traktor.
        return nothing;
    }
    void setBitDepthInBits(const Optional<count>&) override
    {
        // -- This is not supported by Traktor.
    }

    Optional<count> maybeBitRateInKiloBitsPerSecond() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            auto maybeValue = maybeInfoNode->maybeCountValueForAttributeNamed("BITRATE");
            if (maybeValue.isValid()) {
                return { *maybeValue / 1000u };
            }
        }

        return nothing;
    }
    void setBitRateInKiloBitsPerSecond(const Optional<count>& maybeValue) override
    {
        this->p_mutableInfoNode().setCountValueForAttributeNamed(maybeValue.isValid() ? (*maybeValue * 1000u) : Optional<count>{ }, "BITRATE");
    }

    Optional<DecimalNumber> maybeBeatsPerMinute() const override
    {
        auto maybeTempoNode = this->p_maybeTempoNode();
        return maybeTempoNode.isValid() ? maybeTempoNode->maybeDecimalValueForAttributeNamed("BPM") : Optional<DecimalNumber>{ };
    }
    void setBeatsPerMinute(const Optional<DecimalNumber>& maybeValue) override
    {
        if (maybeValue.isValid()) {
            auto tempoNodeAlreadyExisted = this->p_maybeTempoNode().isValid();
            auto mutableTempoNode = this->p_mutableTempoNode();

            mutableTempoNode.setStringValueForAttributeNamed(maybeValue->asString(), "BPM");
            if (!tempoNodeAlreadyExisted) {
                mutableTempoNode.setStringValueForAttributeNamed("100.000000"_String, "BPM_QUALITY");
            }
        }
        else {
            this->p_traktorTrack.deleteSubNodesNamed("TEMPO");
        }
    }

    Optional<Color> maybeColor() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            auto maybeColorIndex = maybeInfoNode->maybeCountValueForAttributeNamed("COLOR");
            if (maybeColorIndex.isValid()) {
                return MutableTrack::p_trackRGBColorForTraktorTrackColorIndex(*maybeColorIndex);
            }
        }

        return { };
    }
    void setColor(const Optional<Color>& maybeValue) override
    {
        this->p_mutableInfoNode().setCountValueForAttributeNamed(maybeValue.isValid() ? MutableTrack::p_traktorTrackColorIndexForTrackRGBColor(*maybeValue) : Optional<count>{ }, "COLOR");
    }

    Optional<count> maybeFileSizeInBytes() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            auto maybeFileSizeInKiloBytes = maybeInfoNode->maybeCountValueForAttributeNamed("FILESIZE");
            if (maybeFileSizeInKiloBytes.isValid()) {
                return (*maybeFileSizeInKiloBytes) * 1024;
            }
        }

        return { };
    }
    void setFileSizeInBytes(const Optional<count>& maybeValue) override
    {
        this->p_mutableInfoNode().setCountValueForAttributeNamed(maybeValue.isValid() ? *maybeValue / 1024u : Optional<count>{ }, "FILESIZE");
    }

    AudioFileType fileType() const override;
    void setFileType(AudioFileType value) override
    {
    }

    Array<String> musicalKeys() const override;
    void setMusicalKeys(const Array<String>&) override;

    Optional<DecimalNumber> maybeLengthInSeconds() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            auto maybeDecimalLengthInSeconds = maybeInfoNode->maybeDecimalValueForAttributeNamed("PLAYTIME_FLOAT");
            if (maybeDecimalLengthInSeconds.isValid()) {
                return *maybeDecimalLengthInSeconds;
            }

            auto maybeLengthInSeconds = maybeInfoNode->maybeCountValueForAttributeNamed("PLAYTIME");
            if (maybeLengthInSeconds.isValid()) {
                return DecimalNumber::withInteger(static_cast<integer64>(*maybeLengthInSeconds));
            }
        }

        return { };
    }
    void setLengthInSeconds(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_mutableInfoNode().setCountValueForAttributeNamed(maybeValue.maybe([](auto value)
                                                                                  {
                                                                                      return static_cast<count>(value.asInteger());
                                                                                  }),
                                                                 "PLAYTIME");
        this->p_mutableInfoNode().setDecimalValueWithFractionDigitsForAttributeNamed(maybeValue, 6, "PLAYTIME_FLOAT");
    }

    Optional<count> maybeTrackNumber() const override
    {
        auto maybeAlbumNode = this->p_maybeAlbumNode();
        return maybeAlbumNode.isValid() ? maybeAlbumNode->maybeCountValueForAttributeNamed("TRACK") : Optional<count>{ };
    }
    void setTrackNumber(const Optional<count>& maybeValue) override
    {
        this->p_mutableAlbumNode().setCountValueForAttributeNamed(maybeValue, "TRACK");
    }

    Optional<count> maybeDiscNumber() const override
    {
        // -- This is not supported by Traktor.
        return nothing;
    }
    void setDiscNumber(const Optional<count>& maybeValue) override
    {
        // -- This is not supported by Traktor.
    }

    Optional<count> maybePlayCount() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            return maybeInfoNode->maybeCountValueForAttributeNamed("PLAYCOUNT");
        }

        return nothing;
    }
    void setPlayCount(const Optional<count>& value) override
    {
        this->p_mutableInfoNode().setCountValueForAttributeNamed(value, "PLAYCOUNT");
    }

    String ratingAsString() const override
    {
        return Common::Track::ratingAsStringFor(*this);
    }
    Optional<Common::TrackRating> maybeRating() const override
    {
        auto maybeInfoNode = this->p_maybeInfoNode();
        if (maybeInfoNode.isValid()) {
            auto maybeRating = maybeInfoNode->maybeCountValueForAttributeNamed("RANKING");
            if (maybeRating.isValid()) {
                return Common::TrackRating::maybeWithValue(*maybeRating);
            }
        }

        return { };
    }
    void setRating(const Optional<Common::TrackRating>& maybeValue) override
    {
        this->p_mutableInfoNode().setCountValueForAttributeNamed(maybeValue.isValid() ? maybeValue->value() : Optional<count>{ }, "RANKING");
    }

    Optional<count> maybeSampleRateInHertz() const override
    {
        // -- This is not supported by Traktor.
        return nothing;
    }
    void setSampleRateInHertz(const Optional<count>&) override
    {
        // -- This is not supported by Traktor.
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
        markers.append(Shared<MutableMarkerOfSomeSort>::with(MutableGridMarker{ DecimalNumber{ }, *this, MutableGridMarker::p_isProtected }));

        return &markers.lastObject()->get<MutableGridMarker>();
    }
    NotNull<Common::MutableLoopMarker *> appendLoopMarker() override
    {
        this->p_markersMayBeModified = true;

        auto& markers = this->p_ensureMarkersAreLoaded();
        markers.append(Shared<MutableMarkerOfSomeSort>::with(MutableLoopMarker{ DecimalNumber{ }, MutableLoopMarker::p_isProtected }));

        return &markers.lastObject()->get<MutableLoopMarker>();
    }
    void removeMarkerAtIndex(count index) override
    {
        this->p_markersMayBeModified = true;

        this->p_ensureMarkersAreLoaded().removeObjectAtIndex(index);
    }
    void setMarkersAndMaybeAddOffsetInSeconds(const Array<Common::MarkerOfSomeSort>&, Optional<DecimalNumber> = nothing) override;

    NXA_VIRTUAL_FOR_TESTING String trackFilePathAsUsedInTraktorPlaylistEntries() const;
};

} }
