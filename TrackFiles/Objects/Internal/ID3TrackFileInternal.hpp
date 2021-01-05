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

#include "Internal/TrackFileInternal.hpp"

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#include <tag/tag.h>
#include <tag/id3v2tag.h>
#include <tag/generalencapsulatedobjectframe.h>
#include <tag/mpegfile.h>
#include <tag/attachedpictureframe.h>
#include <tag/textidentificationframe.h>
#include <tag/popularimeterframe.h>
#include <tag/privateframe.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace NxA {

#define NXA_OBJECT_INTERNAL_CLASS                           ID3TrackFileInternal
#define NXA_OBJECT_INTERNAL_BASE_CLASS                      TrackFileInternal
#define NXA_INTERNAL_OBJECT_DOES_NOT_NEED_EQUAL_OPERATOR

// -- Internal Interface
struct ID3TrackFileInternal : public NXA_OBJECT_INTERNAL_BASE_CLASS
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Types
    struct GeobBodyHeaderStruct {
        byte dummy;
        byte mimeType[25];
        byte filename;
        byte description[16];
        byte majorVersion;
        byte minorVersion;
    };

    struct GeobObjectStruct {
        byte majorVersion;
        byte minorVersion;
        byte data[0];
    };

    // -- Constants
    static constexpr auto id3SeratoMarkersV1FrameDescription = "Serato Markers_";
    static constexpr auto id3SeratoMarkersV2FrameDescription = "Serato Markers2";
    static constexpr auto id3SeratoBeatgridFrameDescription = "Serato BeatGrid";
    static constexpr auto id3RekordBuddyTagsFrameDescription = "Rekord Buddy Tags";

    static constexpr auto id3KeyFrameName = "TKEY";
    static constexpr auto id3ComposerFrameName = "TCOM";
    static constexpr auto id3GroupingFrameName = "TIT1";
    static constexpr auto id3BpmFrameName = "TBPM";
    static constexpr auto id3RecordLabelFrameName = "TPUB";
    static constexpr auto id3RemixerFrameName = "TPE4";
    static constexpr auto id3RatingFrameName = "POPM";
    static constexpr auto id3ArtworkFrameName = "APIC";
    static constexpr auto id3OriginalReleaseTimeFrameName = "TDOR";
    static constexpr auto id3RecordingTimeFrameName = "TDRC";
    static constexpr auto id3ReleaseTimeFrameName = "TDRL";

    // -- Class Methods
    static boolean isAValidGeobFrame(const TagLib::ID3v2::GeneralEncapsulatedObjectFrame& frame)
    {
        auto frameID = frame.frameID();
        if (frameID.size() != 4) {
            return false;
        }

        auto frameIDData = frameID.data();
        if ((frameIDData[0] != 'G') || (frameIDData[1] != 'E') || (frameIDData[2] != 'O') || (frameIDData[3] != 'B')) {
            return false;
        }

        TagLib::String mimeType = frame.mimeType();
        return mimeType == "application/octet-stream";
    }

    static TagLib::ID3v2::FrameList::Iterator frameInListWithDescription(TagLib::ID3v2::FrameList& list, const character* description)
    {
        for (auto it = list.begin(); it != list.end(); ++it) {
            auto frame = static_cast<TagLib::ID3v2::GeneralEncapsulatedObjectFrame*>(*it);
            if (frame->description() == description) {
                return it;
            }
        }

        return list.end();
    }

    static String stringValueForFrameNamedInTag(const character* name, const TagLib::ID3v2::Tag& tag)
    {
        auto frameList = tag.frameList(name);
        for (auto&& frame : frameList) {
            if (frame) {
                auto stringValue = frame->toString();
                if (!stringValue.isEmpty()) {
                    auto value = String(stringValue.to8Bit(true).c_str());
                    if (value.hasNonPrintableCharacters()) {
                        value = String::stringByFilteringNonPrintableCharactersIn(value);
                    }
                    return value;
                }
            }
        }

        return { };
    }

    static String stringValueForGEOBFrameWithDescriptionInTag(const character* description, const TagLib::ID3v2::Tag& tag)
    {
        auto framesList = tag.frameList("GEOB");
        auto framePos = ID3TrackFileInternal::frameInListWithDescription(framesList, description);
        if (framePos != framesList.end()) {
            auto frame = static_cast<const TagLib::ID3v2::GeneralEncapsulatedObjectFrame*>(*framePos);
            if (isAValidGeobFrame(*frame)) {
                auto frameObject = frame->object();
                if (frameObject.size()) {
                    auto headerStruct = reinterpret_cast<GeobObjectStruct*>(frameObject.data());
                    if ((headerStruct->majorVersion == 1) && (headerStruct->minorVersion == 0)) {
                        count size = frameObject.size() - sizeof(GeobObjectStruct);
                        if (size) {
                            String value(reinterpret_cast<const character*>(headerStruct->data), size);
                            if (value.hasNonPrintableCharacters()) {
                                return String::stringByFilteringNonPrintableCharactersIn(value);
                            }

                            return value;
                        }
                    }
                }
            }
        }

        return { };
    }

    static integer integerValueForFrameNamedInTag(const character* name, const TagLib::ID3v2::Tag& tag)
    {
        auto frameList = tag.frameList(name);
        for (auto&& frame : frameList) {
            if (frame) {
                auto stringValue = frame->toString();
                if (!stringValue.isEmpty()) {
                    auto value = String(stringValue.to8Bit(true).c_str());
                    return value.integerValue();
                }
            }
        }

        return 0;
    }

    static integer ratingValueForRatingFrameInTag(const TagLib::ID3v2::Tag& tag)
    {
        auto frameList = tag.frameList(id3RatingFrameName);
        auto frame = frameList.front();
        if (!frame) {
            return -1;
        }

        return static_cast<TagLib::ID3v2::PopularimeterFrame*>(frameList.front())->rating();
    }

    static void setStringValueForFrameNamedInTag(const String& value, const character* name, TagLib::ID3v2::Tag& tag)
    {
        tag.removeFrames(name);
        auto frame = new TagLib::ID3v2::TextIdentificationFrame(name, TagLib::String::Latin1);
        frame->setText(TagLib::StringList(TagLib::String(value.asUTF8(), TagLib::String::UTF8)));
        tag.addFrame(frame);
    }

    static void setStringValueForGEOBFrameWithDescriptionInTag(const String& value, const character* description, TagLib::ID3v2::Tag& tag)
    {
        ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(description, tag, Blob::withStringWithoutTerminator(value), 1, 0);
    }

    static void setIntegerValueForFrameNamedInTag(integer value, const character* name, TagLib::ID3v2::Tag& tag)
    {
        tag.removeFrames(name);
        auto frame = new TagLib::ID3v2::TextIdentificationFrame(name, TagLib::String::Latin1);
        frame->setText(TagLib::StringList(TagLib::String(String::stringWithFormat("%d", value).asUTF8(), TagLib::String::UTF8)));
        tag.addFrame(frame);
    }

    static void setRatingValueForRatingFrameInTag(integer value, TagLib::ID3v2::Tag& tag)
    {
        count counter = 0;
        auto frameList = tag.frameList(id3RatingFrameName);
        auto frame = frameList.front();
        if (frame) {
            counter = static_cast<TagLib::ID3v2::PopularimeterFrame*>(frameList.front())->counter();
        }

        tag.removeFrames(id3RatingFrameName);

        auto popFrame = new TagLib::ID3v2::PopularimeterFrame();
        popFrame->setRating(value);
        popFrame->setCounter(static_cast<unsigned int>(counter));

        tag.addFrame(popFrame);
    }

    static boolean removingPrivateFramesOwnedByInTagDidRemoveSomething(const character* owner, TagLib::ID3v2::Tag& tag)
    {
        auto framesList = tag.frameList("PRIV");
        if (!framesList.size()) {
            return false;
        }

        TagLib::ID3v2::FrameList framesToRemove;

        for (auto&& frame : framesList) {
            auto privateFrame = static_cast<TagLib::ID3v2::PrivateFrame*>(frame);
            if (privateFrame->owner() == owner) {
                framesToRemove.append(frame);
            }
        }

        if (!framesToRemove.size()) {
            return false;
        }

        for (auto&& frame : framesToRemove) {
            tag.removeFrame(frame);
        }

        return true;
    }

    static void removeGEOBFrameWithDescriptionInTag(const character* description, TagLib::ID3v2::Tag& tag)
    {
        auto framesList = tag.frameList("GEOB");
        if (!framesList.size()) {
            return;
        }

        auto frameToDelete = ID3TrackFileInternal::frameInListWithDescription(framesList, description);
        if (frameToDelete != framesList.end()) {
            tag.removeFrame(*frameToDelete);
        }
    }

    static String seratoMarkersV2Base64StringInTag(const TagLib::ID3v2::Tag& tag)
    {
        auto framesList = tag.frameList("GEOB");
        auto framePos = ID3TrackFileInternal::frameInListWithDescription(framesList, id3SeratoMarkersV2FrameDescription);
        if (framePos != framesList.end()) {
            auto frame = static_cast<const TagLib::ID3v2::GeneralEncapsulatedObjectFrame*>(*framePos);
            if (isAValidGeobFrame(*frame)) {
                auto frameObject = frame->object();
                if (frameObject.size()) {
                    auto headerStruct = reinterpret_cast<GeobObjectStruct*>(frameObject.data());
                    if ((headerStruct->majorVersion == 1) && (headerStruct->minorVersion == 1)) {
                        count size = frameObject.size() - sizeof(GeobObjectStruct);
                        if (size) {
                            return { reinterpret_cast<const character*>(headerStruct->data), size };
                        }
                    }
                }
            }
        }

        return { };
    }

    static Array<SeratoMarker::OfSomeSort> seratoMarkersInMarkerV2Base64StringAndTag(const String& markerV2Base64String, const TagLib::ID3v2::Tag& tag)
    {
        MutableArray<SeratoMarker::OfSomeSort> markers;

        auto framesList = tag.frameList("GEOB");

        if (!markerV2Base64String.isEmpty()) {
            TrackFileInternal::addSeratoMarkersV2FromBase64StringTo(markerV2Base64String, markers);
        }
        else {
            auto framePos = ID3TrackFileInternal::frameInListWithDescription(framesList, id3SeratoMarkersV1FrameDescription);
            if (framePos != framesList.end()) {
                auto frame = static_cast<const TagLib::ID3v2::GeneralEncapsulatedObjectFrame*>(*framePos);
                if (isAValidGeobFrame(*frame)) {
                    auto frameObject = frame->object();
                    if (frameObject.size()) {
                        auto headerStruct = reinterpret_cast<GeobObjectStruct*>(frameObject.data());
                        if ((headerStruct->majorVersion == 2) && (headerStruct->minorVersion == 5)) {
                            count size = frameObject.size() - sizeof(GeobObjectStruct);
                            if (size) {
                                TrackFileInternal::addSeratoMarkersV1FromEncodedByteArrayTo(headerStruct->data, size, markers);
                            }
                        }
                    }
                }
            }
        }

        auto framePos = ID3TrackFileInternal::frameInListWithDescription(framesList, id3SeratoBeatgridFrameDescription);
        if (framePos != framesList.end()) {
            auto frame = static_cast<const TagLib::ID3v2::GeneralEncapsulatedObjectFrame*>(*framePos);
            if (isAValidGeobFrame(*frame)) {
                auto frameObject = frame->object();
                if (frameObject.size()) {
                    auto headerStruct = reinterpret_cast<GeobObjectStruct*>(frameObject.data());
                    auto size = frameObject.size();
                    if ((headerStruct->majorVersion == 1) && (headerStruct->minorVersion == 0) && (size > sizeof(GeobObjectStruct))) {
                        size = size - sizeof(GeobObjectStruct);
                        if (size) {
                            TrackFileInternal::addSeratoGridMarkersFromTo(headerStruct->data, size, markers);
                        }
                    }
                }
            }
        }

        return std::move(markers);
    }

    static void replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(const character* description, TagLib::ID3v2::Tag& tag,
                                                                       const Blob& frameData, int majorVersion, int minorVersion)
    {
        ID3TrackFileInternal::removeGEOBFrameWithDescriptionInTag(description, tag);

        if (!frameData.size()) {
            return;
        }

        MutableBlob objectData;

        GeobObjectStruct header;
        header.majorVersion = majorVersion;
        header.minorVersion = minorVersion;
        objectData.appendMemoryWithSize(reinterpret_cast<const byte*>(&header), sizeof(header));
        objectData.append(frameData);

        TagLib::ByteVector newData(reinterpret_cast<char*>(objectData.data().get()), static_cast<unsigned int>(objectData.size()));

        auto newFrame = new TagLib::ID3v2::GeneralEncapsulatedObjectFrame();
        newFrame->setObject(newData);
        newFrame->setTextEncoding(TagLib::String::Latin1);
        newFrame->setMimeType("application/octet-stream");
        newFrame->setFileName("");
        newFrame->setDescription(description);

        tag.addFrame(newFrame);
    }

    static String releaseDateFromTag(const TagLib::ID3v2::Tag& tag)
    {
        auto date = ID3TrackFileInternal::stringValueForFrameNamedInTag(id3RecordingTimeFrameName, tag);
        if (date.isEmpty()) {
            date = ID3TrackFileInternal::stringValueForFrameNamedInTag(id3ReleaseTimeFrameName, tag);
        }
        else if (date.hasPostfix("-00-00")) {
            auto otherDate = ID3TrackFileInternal::stringValueForFrameNamedInTag(id3ReleaseTimeFrameName, tag);
            if ((otherDate.length() == 10) && !otherDate.hasPostfix("-00-00")) {
                date = otherDate;
            }
        }

        return date;
    }

    static void setReleaseDateInTag(const String& date, TagLib::ID3v2::Tag& tag)
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(date, id3RecordingTimeFrameName, tag);
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(date, id3ReleaseTimeFrameName, tag);
    }

    static Blob artworkInTag(const TagLib::ID3v2::Tag& tag)
    {
        auto frameList = tag.frameList(id3ArtworkFrameName);
        if (frameList.size()) {
            const TagLib::ID3v2::AttachedPictureFrame* artworkFrame = nullptr;

            for (const TagLib::ID3v2::Frame* frame : frameList) {
                auto pic = static_cast<const TagLib::ID3v2::AttachedPictureFrame*>(frame);
                if (pic->type() == TagLib::ID3v2::AttachedPictureFrame::FrontCover) {
                    artworkFrame = pic;
                    break;
                }
                else if (pic->type() == TagLib::ID3v2::AttachedPictureFrame::Other) {
                    artworkFrame = pic;
                }
            }

            if (artworkFrame) {
                auto picture = artworkFrame->picture();
                auto size = picture.size();
                if (size) {
                    char *artworkData = picture.data();
                    return Blob::withMemoryAndSize(reinterpret_cast<byte*>(artworkData), size);
                }
            }
        }

        return Blob();
    }

    static void removeArtworkInTag(TagLib::ID3v2::Tag& tag)
    {
        auto frameList = tag.frameList(id3ArtworkFrameName);
        auto frameToRemove = frameList.end();
        if (frameList.size()) {
            for (auto it = frameList.begin(); it != frameList.end(); ++it) {
                auto pic = static_cast<TagLib::ID3v2::AttachedPictureFrame*>(*it);
                if (pic->type() == TagLib::ID3v2::AttachedPictureFrame::FrontCover) {
                    frameToRemove = it;
                    break;
                }
                else if (pic->type() == TagLib::ID3v2::AttachedPictureFrame::Other) {
                    frameToRemove = it;
                }
            }
        }

        if (frameToRemove != frameList.end()) {
            tag.removeFrame(*frameToRemove);
        }
    }

    static void setArtworkInTag(const Blob& artwork, TagLib::ID3v2::Tag& tag)
    {
        ID3TrackFileInternal::removeArtworkInTag(tag);

        if (!artwork.size()) {
            return;
        }

        TagLib::ByteVector data(reinterpret_cast<const char*>(artwork.data().get()), static_cast<unsigned int>(artwork.size()));

        auto newFrame = std::make_unique<TagLib::ID3v2::AttachedPictureFrame>();
        newFrame->setPicture(data);
        newFrame->setType(TagLib::ID3v2::AttachedPictureFrame::FrontCover);
        newFrame->setDescription("");
        newFrame->setTextEncoding(TagLib::String::Latin1);

        tag.addFrame(newFrame.release());
    }

    // -- Instance Variables
    mutable TagLib::ID3v2::Tag *id3v2Tag;

    // -- Constructor & Destructors
    ID3TrackFileInternal(const FilePath& path) : TrackFileInternal(path), id3v2Tag(nullptr) { }
    ~ID3TrackFileInternal() override = default;

    // -- Instance Methods
    void removePrivateFramesOwnedByIfAny(const String& owner)
    {
        if (!this->id3v2Tag) {
            return;
        }

        ID3TrackFileInternal::removingPrivateFramesOwnedByInTagDidRemoveSomething(owner.asUTF8(), *this->id3v2Tag);
    }

    // -- Overridden TrackFileInternal Instance Methods
    boolean save() const override
    {
        return this->TrackFileInternal::save();
    }

    String releaseDate() const override
    {
        return ID3TrackFileInternal::releaseDateFromTag(*this->id3v2Tag);
    }
    void setReleaseDate(const String& releaseDate) override
    {
        ID3TrackFileInternal::setReleaseDateInTag(releaseDate, *this->id3v2Tag);
    }

    boolean hasKey() const override
    {
        return true;
    }
    String key() const override
    {
        return ID3TrackFileInternal::stringValueForFrameNamedInTag(id3KeyFrameName, *this->id3v2Tag);
    }
    void setKey(const String& musicalKey) override
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(musicalKey, id3KeyFrameName, *this->id3v2Tag);
    }

    String composer() const override
    {
        return ID3TrackFileInternal::stringValueForFrameNamedInTag(id3ComposerFrameName, *this->id3v2Tag);
    }
    void setComposer(const String& composer) override
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(composer, id3ComposerFrameName, *this->id3v2Tag);
    }

    String grouping() const override
    {
        return ID3TrackFileInternal::stringValueForFrameNamedInTag(id3GroupingFrameName, *this->id3v2Tag);
    }
    void setGrouping(const String& grouping) override
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(grouping, id3GroupingFrameName, *this->id3v2Tag);
    }

    String bpm() const override
    {
        return ID3TrackFileInternal::stringValueForFrameNamedInTag(id3BpmFrameName, *this->id3v2Tag);
    }
    void setBpm(const String& beatsPerMinute) override
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(TrackFileInternal::roundedBpmValueFromValue(beatsPerMinute),
                                                               id3BpmFrameName, *this->id3v2Tag);
    }

    boolean hasRecordLabel() const override
    {
        return true;
    }
    String recordLabel() const override
    {
        return ID3TrackFileInternal::stringValueForFrameNamedInTag(id3RecordLabelFrameName, *this->id3v2Tag);
    }
    void setRecordLabel(const String& recordLabel) override
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(recordLabel, id3RecordLabelFrameName, *this->id3v2Tag);
    }

    boolean hasRemixer() const override
    {
        return true;
    }
    String remixer() const override
    {
        return ID3TrackFileInternal::stringValueForFrameNamedInTag(id3RemixerFrameName, *this->id3v2Tag);
    }
    void setRemixer(const String& remixer) override
    {
        ID3TrackFileInternal::setStringValueForFrameNamedInTag(remixer, id3RemixerFrameName, *this->id3v2Tag);
    }

    Optional<uinteger> maybeRating() const override
    {
        auto ratingAsInteger = ID3TrackFileInternal::ratingValueForRatingFrameInTag(*this->id3v2Tag);
        if (ratingAsInteger < 0) {
            return nothing;
        }

        return ratingAsInteger;
    }
    void setRating(const Optional<uinteger>& maybeRating) override
    {
        ID3TrackFileInternal::setRatingValueForRatingFrameInTag(maybeRating.isValid() ? *maybeRating : -1,
                                                                *this->id3v2Tag);
    }

    String tags(void) const override
    {
        return ID3TrackFileInternal::stringValueForGEOBFrameWithDescriptionInTag(id3RekordBuddyTagsFrameDescription, *this->id3v2Tag);
    }
    void setTags(const String& tags) override
    {
        ID3TrackFileInternal::setStringValueForGEOBFrameWithDescriptionInTag(tags, id3RekordBuddyTagsFrameDescription, *this->id3v2Tag);
    }

    Blob artwork() const override
    {
        return ID3TrackFileInternal::artworkInTag(*this->id3v2Tag);
    }
    void setArtwork(const Blob& artwork) override
    {
        ID3TrackFileInternal::setArtworkInTag(artwork, *this->id3v2Tag);
    }

    boolean seratoBeatGridIsLocked() const override
    {
        auto base64String = ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag);
        return ID3TrackFileInternal::seratoBeatGridLockFlagFromMarkerV2Data(base64String);
    }
    void setSeratoBeatGridIsLocked(boolean seratoBeatGridIsLocked) override
    {
        auto decodedData = MutableBlob::withBase64String(ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag));
        TrackFileInternal::setSeratoBeatGridLockFlagInDecodedMarkerV2Data(seratoBeatGridIsLocked, decodedData);
        ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(id3SeratoMarkersV2FrameDescription, *this->id3v2Tag, Blob::withStringWithoutTerminator(decodedData.base64String()), 1, 1);
    }

    Array<SeratoMarker::OfSomeSort> seratoMarkers() const override
    {
        auto base64String = ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag);
        return ID3TrackFileInternal::seratoMarkersInMarkerV2Base64StringAndTag(base64String, *this->id3v2Tag);
    }
    void setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers) override
    {
        auto decodedData = MutableBlob::withBase64String(ID3TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->id3v2Tag));
        ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(id3SeratoMarkersV1FrameDescription, *this->id3v2Tag, TrackFileInternal::markersV1Id3EncodedBlobFrom(seratoMarkers), 2, 5);
        ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(id3SeratoBeatgridFrameDescription, *this->id3v2Tag, SeratoGridMarker::gridMarkerDataFrom(seratoMarkers), 1, 0);
        decodedData = TrackFileInternal::seratoMarkersV2DecodedDataFromAndOldMarkersV2DecodedData(seratoMarkers, decodedData);
        ID3TrackFileInternal::replaceGEOBFrameWithDescriptionInTagWithDataAndVersion(id3SeratoMarkersV2FrameDescription, *this->id3v2Tag, Blob::withStringWithoutTerminator(decodedData.base64String()), 1, 1);
    }
};

}
