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

#include <TrackFiles/FLACTrackFile.hpp>

#include "Internal/TrackFileInternal.hpp"
#include "Internal/ID3TrackFileInternal.hpp"
#include "Internal/OGGTrackFileInternal.hpp"

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#include <tag/flacfile.h>
#include <tag/privateframe.h>
#include <tag/id3v2tag.h>
#include <tag/xiphcomment.h>
#include <tag/flacproperties.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace NxA {

#define NXA_OBJECT_CLASS                                    FLACTrackFile
#define NXA_OBJECT_INTERNAL_BASE_CLASS                      TrackFileInternal

// -- Internal Interface
class FLACTrackFileInternal : public NXA_OBJECT_INTERNAL_BASE_CLASS
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Types
    struct FLACSeratoMarkersHeaderStruct {
        byte mimeType[25];
        byte filename[1];
        byte description[16];
        byte majorVersion;
        byte minorVersion;
        byte data[0];
    };

    // -- Constants
    static constexpr auto flacSeratoMarkersV1ItemName = "SERATO_MARKERS";
    static constexpr auto flacSeratoMarkersV2ItemName = "SERATO_MARKERS_V2";
    static constexpr auto flacSeratoBeatgridItemName = "SERATO_BEATGRID";

    // -- Instance Variables
    mutable std::unique_ptr<TagLib::FLAC::File> flacFile;
    mutable TagLib::ID3v2::Tag* id3v2Tag;
    mutable TagLib::Ogg::XiphComment* xiphComment;

    // -- Constructor & Destructors
    FLACTrackFileInternal(const FilePath& path) : TrackFileInternal(path), id3v2Tag(nullptr), xiphComment(nullptr) { }
    ~FLACTrackFileInternal() override = default;

    // -- Operators
    inline bool operator==(const FLACTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator==(other);
    }
    inline bool operator<(const FLACTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator<(other);
    }

    // -- Instance Methods
    String seratoMarkersV2Base64StringInComment() const
    {
        auto& fieldListMap = this->xiphComment->fieldListMap();

        auto markersEncodedData = fieldListMap[flacSeratoMarkersV2ItemName].toString();
        auto markersEncodedDataSize = markersEncodedData.size();
        if (markersEncodedDataSize) {
            auto decodedMarkersData = Blob::withBase64String(String(markersEncodedData.data(TagLib::String::UTF8).data(), markersEncodedDataSize));
            auto headerStruct = reinterpret_cast<const FLACSeratoMarkersHeaderStruct*>(decodedMarkersData.data().get());
            if ((headerStruct->majorVersion == 1) && (headerStruct->minorVersion == 1)) {
                return { reinterpret_cast<const character*>(headerStruct->data), decodedMarkersData.size() - sizeof(FLACSeratoMarkersHeaderStruct) };
            }
        }

        return { };
    }

    void addSeratoMarkersInMarkerV2Base64StringAndCommentTo(const String& base64String, MutableArray<SeratoMarker::OfSomeSort>& markers) const
    {
        if (!base64String.isEmpty()) {
            TrackFileInternal::addSeratoMarkersV2FromBase64StringTo(base64String, markers);
        }

        auto& fieldListMap = this->xiphComment->fieldListMap();

        auto beatGridEncodedData = fieldListMap[flacSeratoBeatgridItemName].toString();
        auto encodedBeatGridDataSize = beatGridEncodedData.size();
        if (encodedBeatGridDataSize) {
            auto decodedGridMarkersData = Blob::withBase64String(String(beatGridEncodedData.data(TagLib::String::UTF8).data(), encodedBeatGridDataSize));
            auto headerStruct = reinterpret_cast<const FLACSeratoMarkersHeaderStruct*>(decodedGridMarkersData.data().get());
            auto size = decodedGridMarkersData.size();
            if ((headerStruct->majorVersion == 1) && (headerStruct->minorVersion == 0) && (size > sizeof(FLACSeratoMarkersHeaderStruct))) {
                SeratoGridMarker::addMarkersWithMemoryAtTo(headerStruct->data, size - sizeof(FLACSeratoMarkersHeaderStruct), markers);
            }
        }
    }

    void setSeratoMarkerDataFieldInCommentTo(const character* itemName, const character* description,
                                             const Blob& data, byte majorVersion, byte minorVersion) const
    {
        FLACSeratoMarkersHeaderStruct header;
        memcpy(header.mimeType, "application/octet-stream", 25);
        header.filename[0] = 0;
        memcpy(header.description, description, 16);
        header.majorVersion = majorVersion;
        header.minorVersion = minorVersion;

        MutableBlob decodedData;
        decodedData.appendMemoryWithSize(reinterpret_cast<const byte*>(&header), sizeof(header));
        decodedData.append(data);

        auto encodedData = Blob::base64StringFor(decodedData.data(), decodedData.size());
        this->xiphComment->addField(itemName, TagLib::String(encodedData.asUTF8(), TagLib::String::UTF8), true);
    }

    void setSeratoGridMarkersItemInComment(const MutableArray<SeratoMarker::OfSomeSort>& markers) const
    {
        auto gridMarkerData = SeratoGridMarker::gridMarkerDataFrom(markers);
        if (!gridMarkerData.size()) {
            this->xiphComment->removeFields(flacSeratoBeatgridItemName);
            return;
        }

        this->setSeratoMarkerDataFieldInCommentTo(flacSeratoBeatgridItemName, "Serato BeatGrid", gridMarkerData, 1, 0);
    }

    void removePrivateFramesOwnedByIfAny(const String& owner)
    {
        if (!this->id3v2Tag) {
            return;
        }

        ID3TrackFileInternal::removingPrivateFramesOwnedByInTagDidRemoveSomething(owner.asUTF8(), *this->id3v2Tag);
    }

    void removeFieldNamedIfAny(const String& name)
    {
        if (!this->xiphComment) {
            return;
        }

        OGGTrackFileInternal::removingFieldNamedInCommentDidRemoveSomething(name, *this->xiphComment);
    }

    // -- Overridden TrackFileInternal Instance Methods
    boolean parse() const override
    {
        this->load();

        this->flacFile = std::make_unique<TagLib::FLAC::File>(this->fileStream.get(), TagLib::ID3v2::FrameFactory::instance(),
                                                              true, TagLib::AudioProperties::ReadStyle::Fast);
        if (!this->flacFile->isValid()) {
            this->flacFile = nullptr;
            return false;
        }

        this->file = this->flacFile.get();

        if (this->flacFile->hasXiphComment()) {
            this->xiphComment = this->flacFile->xiphComment();
        }
        else if (this->flacFile->hasID3v2Tag()) {
            this->id3v2Tag = this->flacFile->ID3v2Tag();
        }

        if (!this->id3v2Tag && !this->xiphComment) {
            this->flacFile = nullptr;
            return false;
        }

        return this->TrackFileInternal::parse();
    }
    boolean save() const override
    {
        if (!this->flacFile.get()) {
            return true;
        }

        return this->TrackFileInternal::save();
    }

    AudioFileType type() const override
    {
        return AudioFileType::FLAC;
    }

    String releaseDate() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::releaseDateInComment(*this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::releaseDateFromTag(*this->id3v2Tag);
        }
    }
    void setReleaseDate(const String& releaseDate) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setReleaseDateInComment(releaseDate, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setReleaseDateInTag(releaseDate, *this->id3v2Tag);
        }
    }

    boolean hasKey() const override
    {
        return true;
    }
    String key() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggKeyFieldName, *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForFrameNamedInTag(ID3TrackFileInternal::id3KeyFrameName, *this->id3v2Tag);
        }
    }
    void setKey(const String& musicalKey) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(musicalKey, OGGTrackFileInternal::oggKeyFieldName, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForFrameNamedInTag(musicalKey, ID3TrackFileInternal::id3KeyFrameName, *this->id3v2Tag);
        }
    }

    String composer() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggComposerFieldName, *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForFrameNamedInTag(ID3TrackFileInternal::id3ComposerFrameName, *this->id3v2Tag);
        }
    }
    void setComposer(const String& composer) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(composer, OGGTrackFileInternal::oggComposerFieldName, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForFrameNamedInTag(composer, ID3TrackFileInternal::id3ComposerFrameName, *this->id3v2Tag);
        }
    }

    String grouping() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggGroupingFieldName, *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForFrameNamedInTag(ID3TrackFileInternal::id3GroupingFrameName, *this->id3v2Tag);
        }
    }
    void setGrouping(const String& grouping) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(grouping, OGGTrackFileInternal::oggGroupingFieldName, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForFrameNamedInTag(grouping, ID3TrackFileInternal::id3GroupingFrameName, *this->id3v2Tag);
        }
    }

    String bpm() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggBpmFieldName, *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForFrameNamedInTag(ID3TrackFileInternal::id3BpmFrameName, *this->id3v2Tag);
        }
    }
    void setBpm(const String& beatsPerMinute) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(beatsPerMinute, OGGTrackFileInternal::oggBpmFieldName, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForFrameNamedInTag(beatsPerMinute, ID3TrackFileInternal::id3BpmFrameName, *this->id3v2Tag);
        }
    }

    boolean hasRecordLabel() const override
    {
        return true;
    }
    String recordLabel() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggRecordLabelFieldName, *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForFrameNamedInTag(ID3TrackFileInternal::id3RecordLabelFrameName, *this->id3v2Tag);
        }
    }
    void setRecordLabel(const String& recordLabel) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(recordLabel, OGGTrackFileInternal::oggRecordLabelFieldName, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForFrameNamedInTag(recordLabel, ID3TrackFileInternal::id3RecordLabelFrameName, *this->id3v2Tag);
        }
    }

    boolean hasRemixer() const override
    {
        return true;
    }
    String remixer() const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggRemixerFieldName, *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForFrameNamedInTag(ID3TrackFileInternal::id3RemixerFrameName, *this->id3v2Tag);
        }
    }
    void setRemixer(const String& remixer) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(remixer, OGGTrackFileInternal::oggRemixerFieldName, *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForFrameNamedInTag(remixer, ID3TrackFileInternal::id3RemixerFrameName, *this->id3v2Tag);
        }
    }

    Optional<uinteger> maybeRating() const override
    {
        if (this->xiphComment) {
            auto ratingAsString = OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggRatingFieldName,
                                                                                          *this->xiphComment);
            if (!ratingAsString.length()) {
                return nothing;
            }

            return ratingAsString.integerValue();
        }
        else {
            auto ratingAsInteger = ID3TrackFileInternal::ratingValueForRatingFrameInTag(*this->id3v2Tag);
            if (ratingAsInteger < 0) {
                return nothing;
            }

            return ratingAsInteger;
        }
    }
    void setRating(const Optional<uinteger>& maybeRating) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(maybeRating.isValid() ?
                                                                       String::stringWithFormat("%d", *maybeRating) : String{ },
                                                                       OGGTrackFileInternal::oggRatingFieldName,
                                                                       *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setRatingValueForRatingFrameInTag(maybeRating.isValid() ? *maybeRating : -1,
                                                                    *this->id3v2Tag);
        }
    }

    String tags(void) const override
    {
        if (this->xiphComment) {
            return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggRekordbuddyTagsFieldName,
                                                                           *this->xiphComment);
        }
        else {
            return ID3TrackFileInternal::stringValueForGEOBFrameWithDescriptionInTag(ID3TrackFileInternal::id3RekordBuddyTagsFrameDescription,
                                                                                     *this->id3v2Tag);
        }
    }
    void setTags(const String& tags) override
    {
        if (this->xiphComment) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(tags, OGGTrackFileInternal::oggRekordbuddyTagsFieldName,
                                                                       *this->xiphComment);
        }
        else {
            ID3TrackFileInternal::setStringValueForGEOBFrameWithDescriptionInTag(tags,
                                                                                 ID3TrackFileInternal::id3RekordBuddyTagsFrameDescription,
                                                                                 *this->id3v2Tag);
        }
    }

    Blob artwork() const override
    {
        if (this->xiphComment) {
            // -- TODO: Artwork to be implemented.
            return { };
        }
        else {
            return ID3TrackFileInternal::artworkInTag(*this->id3v2Tag);
        }
    }
    void setArtwork(const Blob& artwork) override
    {
        if (this->xiphComment) {
            // -- TODO: Artwork to be implemented.
        }
        else {
            ID3TrackFileInternal::setArtworkInTag(artwork, *this->id3v2Tag);
        }
    }

    boolean hasBitDepth() const override
    {
        return true;
    }
    count bitDepthInBits() const override
    {
        return static_cast<TagLib::FLAC::Properties*>(this->audioProperties)->bitsPerSample();
    }

    boolean seratoBeatGridIsLocked() const override
    {
        String base64String;

        if (this->xiphComment) {
            base64String = this->seratoMarkersV2Base64StringInComment();
        }
        else {
            base64String = ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag);
        }

        return TrackFileInternal::seratoBeatGridLockFlagFromMarkerV2Data(base64String);
    }
    void setSeratoBeatGridIsLocked(boolean seratoBeatGridIsLocked) override
    {
        if (this->xiphComment) {
            auto decodedData = MutableBlob::withBase64String(this->seratoMarkersV2Base64StringInComment());
            TrackFileInternal::setSeratoBeatGridLockFlagInDecodedMarkerV2Data(seratoBeatGridIsLocked, decodedData);
            if (!decodedData.size()) {
                this->xiphComment->removeFields(flacSeratoMarkersV2ItemName);
            }
            else {
                this->setSeratoMarkerDataFieldInCommentTo(flacSeratoMarkersV2ItemName, "Serato Markers2",
                                                          Blob::withStringWithoutTerminator(decodedData.base64String()), 1, 1);
            }
        }
        else {
            auto decodedData = MutableBlob::withBase64String(ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag));
            TrackFileInternal::setSeratoBeatGridLockFlagInDecodedMarkerV2Data(seratoBeatGridIsLocked, decodedData);
            ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(ID3TrackFileInternal::id3SeratoMarkersV2FrameDescription,
                                                                                         *this->id3v2Tag,
                                                                                         Blob::withStringWithoutTerminator(decodedData.base64String()), 1, 1);
        }
    }

    Array<SeratoMarker::OfSomeSort> seratoMarkers() const override
    {
        String base64String;

        if (this->xiphComment) {
            base64String = this->seratoMarkersV2Base64StringInComment();
            MutableArray<SeratoMarker::OfSomeSort> markers;
            this->addSeratoMarkersInMarkerV2Base64StringAndCommentTo(base64String, markers);
            return std::move(markers);
        }
        else {
            base64String = ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag);
            return ID3TrackFileInternal::seratoMarkersInMarkerV2Base64StringAndTag(base64String, *this->id3v2Tag);
        }
    }
    void setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers) override
    {
        if (this->xiphComment) {
            auto decodedData = MutableBlob::withBase64String(this->seratoMarkersV2Base64StringInComment());
            this->xiphComment->removeFields(flacSeratoMarkersV1ItemName);
            this->setSeratoGridMarkersItemInComment(seratoMarkers);
            decodedData = TrackFileInternal::seratoMarkersV2DecodedDataFromAndOldMarkersV2DecodedData(seratoMarkers, decodedData);
            if (!decodedData.size()) {
                this->xiphComment->removeFields(flacSeratoMarkersV2ItemName);
            }
            else {
                this->setSeratoMarkerDataFieldInCommentTo(flacSeratoMarkersV2ItemName, "Serato Markers2",
                                                          Blob::withStringWithoutTerminator(decodedData.base64String()), 1, 1);
            }
        }
        else {
            auto decodedData = MutableBlob::withBase64String(ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag));
            ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(ID3TrackFileInternal::id3SeratoMarkersV1FrameDescription, *this->id3v2Tag, TrackFileInternal::markersV1Id3EncodedBlobFrom(seratoMarkers), 2, 5);
            ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(ID3TrackFileInternal::id3SeratoBeatgridFrameDescription, *this->id3v2Tag, SeratoGridMarker::gridMarkerDataFrom(seratoMarkers), 1, 0);
            decodedData = TrackFileInternal::seratoMarkersV2DecodedDataFromAndOldMarkersV2DecodedData(seratoMarkers, decodedData);
            ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(ID3TrackFileInternal::id3SeratoMarkersV2FrameDescription, *this->id3v2Tag, Blob::withStringWithoutTerminator(decodedData.base64String()), 1, 1);
        }
    }
};

}
