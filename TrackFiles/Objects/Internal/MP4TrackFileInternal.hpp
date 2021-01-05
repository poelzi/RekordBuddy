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

#include <TrackFiles/MP4TrackFile.hpp>

#include "Internal/TrackFileInternal.hpp"

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#include <tag/mp4tag.h>
#include <tag/mp4file.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace NxA {

// -- Private Variables

static DecimalNumber p_mp4OffsetToAddToMarkerPositionsForRekordboxInSeconds("0.048");

#define NXA_OBJECT_CLASS                                    MP4TrackFile
#define NXA_OBJECT_INTERNAL_BASE_CLASS                      TrackFileInternal

// -- Internal Interface
class MP4TrackFileInternal : public NXA_OBJECT_INTERNAL_BASE_CLASS
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Types
    struct MP4SeratoMarkersHeaderStruct {
        byte mimeType[25];
        byte filename[1];
        byte description[16];
        byte majorVersion;
        byte minorVersion;
        byte data[0];
    };

    // -- Constants
    static constexpr auto mp4SeratoMarkersV1ItemName = "----:com.serato.dj:markers";
    static constexpr auto mp4SeratoMarkersV2ItemName = "----:com.serato.dj:markersv2";
    static constexpr auto mp4SeratoBeatgridItemName = "----:com.serato.dj:beatgrid";
    static constexpr auto mp4KeyItemName = "----:com.apple.iTunes:initialkey";
    static constexpr auto mp4PublisherItemName = "----:com.apple.iTunes:publisher";
    static constexpr auto mp4LabelItemName = "----:com.apple.iTunes:LABEL";
    static constexpr auto mp4ComposerItemName = "\251wrt";
    static constexpr auto mp4GroupingItemName = "\251grp";
    static constexpr auto mp4YearItemName = "\251day";
    static constexpr auto mp4BpmItemName = "tmpo";
    static constexpr auto mp4ArtworkItemName = "covr";
    static constexpr auto mp4RatingItemName = "----:com.apple.iTunes:rating wmp";
    static constexpr auto mp4RekordBuddyTagsItemName = "----:audio.next.RekordBuddy:TAGS";

    static constexpr auto mp4SeratoMarkersV1FrameDescription = "Serato Markers_";
    static constexpr auto mp4SeratoMarkersV2FrameDescription = "Serato Markers2";
    static constexpr auto mp4SeratoBeatgridFrameDescription = "Serato Beatgrid";

    // -- Class Methods
#if 0
    static void listAtomsBetweenStartAndEnd(const byte* start, const byte* end)
    {
        while (start < end) {
            printf("%c%c%c%c\n", start[4], start[5], start[6], start[7]);

            uinteger32 atomSize = Platform::bigEndianUInteger32ValueAt(start);
            if (!atomSize) {
                return;
            }

            start += atomSize;
        }
    }
#endif

    static boolean foundAtomWithIDInStream(uinteger32 atomID, TagLib::FileStream& fileStream)
    {
        while (fileStream.tell() < fileStream.length()) {
            auto header = fileStream.readBlock(8);

            uinteger32 foundAtomID = Platform::bigEndianUInteger32ValueAt(header.data() + 4);
            if (foundAtomID == atomID) {
                return true;
            }

            uinteger32 atomSize = Platform::bigEndianUInteger32ValueAt(header.data());
            if (atomSize <= 8) {
                return false;
            }

            fileStream.seek(atomSize - 8, TagLib::FileStream::Position::Current);
        }

        return false;
    }

    static void fileInDataIsAppleLosslessOrStem(TagLib::FileStream& fileStream, boolean* lossless, boolean* stem)
    {
        *lossless = false;
        *stem = false;

        if (MP4TrackFileInternal::foundAtomWithIDInStream('moov', fileStream)) {
            long positionBackup = fileStream.tell();

            if (MP4TrackFileInternal::foundAtomWithIDInStream('udta', fileStream)) {
                if (MP4TrackFileInternal::foundAtomWithIDInStream('stem', fileStream)) {
                    *stem = true;
                }
            }

            fileStream.seek(positionBackup, TagLib::FileStream::Position::Beginning);

            if (MP4TrackFileInternal::foundAtomWithIDInStream('trak', fileStream)) {
                if (MP4TrackFileInternal::foundAtomWithIDInStream('mdia', fileStream)) {
                    if (MP4TrackFileInternal::foundAtomWithIDInStream('minf', fileStream)) {
                        if (MP4TrackFileInternal::foundAtomWithIDInStream('stbl', fileStream)) {
                            if (MP4TrackFileInternal::foundAtomWithIDInStream('stsd', fileStream)) {
                                auto alacData = fileStream.readBlock(16);
                                *lossless = Platform::bigEndianUInteger32ValueAt(alacData.data() + 12) == 'alac';
                            }
                        }
                    }
                }
            }
        }
    }

    static integer integerValueForItemNamedInTag(const character* name, const TagLib::MP4::Tag& tag)
    {
        auto item = tag.item(name);
        if (!item.isValid()) {
            return 0;
        }
        return item.toInt();
    }

    static String stringValueForItemNamedInTag(const character* name, const TagLib::MP4::Tag& tag)
    {
        auto item = tag.item(name);
        if (!item.isValid()) {
            return { };
        }
        auto stringList = item.toStringList();
        if (stringList.size() == 0) {
            return { };
        }

        String value(stringList.front().to8Bit(true).c_str());
        if (value.hasNonPrintableCharacters()) {
            return String::stringByFilteringNonPrintableCharactersIn(value);
        }

        return value;
    }

    static void setIntegerValueForItemNamedInTag(integer value, const character* name, TagLib::MP4::Tag& tag)
    {
        auto newItem = std::make_unique<TagLib::MP4::Item>(value);
        tag.setItem(name, *newItem);
    }

    static void setStringValueForItemNamedInTag(const String& value, const character* name, TagLib::MP4::Tag& tag)
    {
        auto newString = TagLib::String(value.asUTF8(), TagLib::String::UTF8);

        auto item = tag.item(name);
        if (item.isValid()) {
            auto stringList = item.toStringList();
            if (stringList.size() > 1) {
                stringList.erase(stringList.begin());
                stringList.insert(stringList.begin(), newString);

                auto newItem = std::make_unique<TagLib::MP4::Item>(stringList);
                tag.setItem(name, *newItem);
                return;
            }
        }

        auto newItem = std::make_unique<TagLib::MP4::Item>(TagLib::StringList(newString));
        tag.setItem(name, *newItem);
    }

    static String seratoMarkersV2Base64StringInTag(const TagLib::MP4::Tag& tag)
    {
        auto markersItem = tag.item(mp4SeratoMarkersV2ItemName);
        if (!markersItem.isValid() || (markersItem.atomDataType() != TagLib::MP4::AtomDataType::TypeUTF8)) {
            return { };
        }

        TagLib::String base64String(markersItem.toStringList().toString());
        auto base64StringSize = base64String.size();
        if (!base64StringSize) {
            return { };
        }

        auto base64SubString = String(base64String.data(TagLib::String::UTF8).data(), base64StringSize);
        auto decodedData = Blob::withBase64String(base64SubString);
        auto headerStruct = reinterpret_cast<const MP4SeratoMarkersHeaderStruct*>(decodedData.data().get());
        if ((headerStruct->majorVersion != 1) || (headerStruct->minorVersion != 1)) {
            return { };
        }

        count size = decodedData.size() - sizeof(MP4SeratoMarkersHeaderStruct);
        if (!size) {
            return { };
        }

        return { reinterpret_cast<const character*>(headerStruct->data), size };
    }

    static Array<SeratoMarker::OfSomeSort> seratoMarkersInTag(const TagLib::MP4::Tag& tag)
    {
        MutableArray<SeratoMarker::OfSomeSort> markers;

        auto base64String = MP4TrackFileInternal::seratoMarkersV2Base64StringInTag(tag);
        if (!base64String.isEmpty()) {
            MP4TrackFileInternal::addSeratoMarkersV2FromBase64StringTo(base64String, markers);
        }
        else {
            auto markersV1Item = tag.item(mp4SeratoMarkersV1ItemName);
            if (markersV1Item.isValid() && (markersV1Item.atomDataType() == TagLib::MP4::AtomDataType::TypeUTF8)) {
                auto encodedData = markersV1Item.toStringList().toString();

                auto encodedDataSize = encodedData.size();
                if (encodedDataSize) {
                    auto decodedData = Blob::withBase64String(String(encodedData.data(TagLib::String::UTF8).data(), encodedDataSize));
                    auto headerStruct = reinterpret_cast<const MP4SeratoMarkersHeaderStruct*>(decodedData.data().get());
                    if ((headerStruct->majorVersion == 2) && (headerStruct->minorVersion == 5)) {
                        MP4TrackFileInternal::addSeratoMarkersV1FromRawByteArrayTo(headerStruct->data, decodedData.size() - sizeof(MP4SeratoMarkersHeaderStruct), markers);
                    }
                }
            }
        }

        auto beatgridItem = tag.item(mp4SeratoBeatgridItemName);
        if (beatgridItem.isValid() && (beatgridItem.atomDataType() == TagLib::MP4::AtomDataType::TypeUTF8)) {
            auto encodedData = beatgridItem.toStringList().toString();

            auto encodedDataSize = encodedData.size();
            if (encodedDataSize) {
                auto decodedData = Blob::withBase64String(String(encodedData.data(TagLib::String::UTF8).data(), encodedDataSize));
                auto headerStruct = reinterpret_cast<const MP4SeratoMarkersHeaderStruct*>(decodedData.data().get());
                auto size = decodedData.size();
                if ((headerStruct->majorVersion == 1) && (headerStruct->minorVersion == 0) && (size > sizeof(MP4SeratoMarkersHeaderStruct))) {
                    MP4TrackFileInternal::addSeratoGridMarkersFromTo(headerStruct->data, size - sizeof(MP4SeratoMarkersHeaderStruct), markers);
                }
            }
        }

        return std::move(markers);
    }

    // -- Instance Variables
    mutable std::unique_ptr<TagLib::MP4::File> mp4File;
    mutable TagLib::MP4::Tag* mp4Tag;

    mutable Optional<AudioFileType> maybeType;

    // -- Constructor & Destructors
    MP4TrackFileInternal(const FilePath& path) : TrackFileInternal(path), mp4Tag(nullptr) { }
    ~MP4TrackFileInternal() override = default;

    // -- Operators
    inline bool operator==(const MP4TrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator==(other);
    }
    inline bool operator<(const MP4TrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator<(other);
    }

    // -- Instance Methods
    void removeItemNamedIfAny(const String& name)
    {
        if (!this->mp4Tag) {
            return;
        }

        // -- For MP4 files we don't check if the item is there or not because we might to remove an item that
        // -- does not get loaded properly and therefore is not there but will also no get saved if the
        // -- metadata is marked as modified, thus achieving the same result for now. This will be fixed
        this->mp4Tag->removeItem(name.asUTF8());
    }

    void replaceFrameNamedWithDataAndVersion(const character* frameName, const character* frameDescription, const Blob& frameData,
                                             integer majorVersion, integer minorVersion) const
    {
        if (!this->mp4Tag) {
            return;
        }

        TagLib::MP4::Tag& tag = *this->mp4Tag;
        tag.removeItem(frameName);

        if (!frameData.size()) {
            return;
        }

        MutableBlob decodedData;

        MP4SeratoMarkersHeaderStruct header;
        memcpy(header.mimeType, "application/octet-stream", 25);
        header.filename[0] = 0;
        memcpy(header.description, frameDescription, 16);
        header.majorVersion = majorVersion;
        header.minorVersion = minorVersion;
        decodedData.appendMemoryWithSize(reinterpret_cast<byte*>(&header), sizeof(header));
        decodedData.append(frameData);

        auto encodedData = Blob::base64StringFor(decodedData.data(), decodedData.size());
        TagLib::String newString(encodedData.asUTF8(), TagLib::String::UTF8);
        TagLib::StringList newList(newString);

        TagLib::MP4::Item newItem(newList);
        newItem.setAtomDataType(TagLib::MP4::AtomDataType::TypeUTF8);
        NXA_ASSERT_TRUE(newItem.isValid());

        tag.setItem(frameName, newItem);
    }

    // -- Overridden TrackFileInternal Instance Methods
    boolean parse() const override
    {
        this->load();

        this->mp4File = std::make_unique<TagLib::MP4::File>(this->fileStream.get(),
                                                            true, TagLib::AudioProperties::ReadStyle::Fast);
        if (!this->mp4File->isValid()) {
            this->mp4File = nullptr;
            return false;
        }

        this->file = this->mp4File.get();

        this->mp4Tag = this->mp4File->tag();
        if (!this->mp4Tag) {
            this->mp4File = nullptr;
            return false;
        }

        return this->TrackFileInternal::parse();
    }
    boolean save() const override
    {
        if (!this->mp4File) {
            return true;
        }

        return this->TrackFileInternal::save();
    }

    AudioFileType type() const override
    {
        if (!this->maybeType.isValid()) {
            this->load();

            NxA::boolean lossless = false;
            NxA::boolean stem = false;

            MP4TrackFileInternal::fileInDataIsAppleLosslessOrStem(*this->fileStream, &lossless, &stem);

            if (stem) {
                this->maybeType = lossless ? AudioFileType::ALACSTEM : AudioFileType::AACSTEM;
            }
            else if (lossless) {
                this->maybeType = AudioFileType::ALAC;
            }
            else {
                static FilePath p_m4aExtension{ "m4a"_String };
                if (this->filePath.hasExtension(p_m4aExtension, FilePath::CaseSensitivity::None)) {
                    // -- We're not sure if AAC files can have an MP4 extension. If that's the case
                    // -- then this assumption below in incorrect.
                    this->maybeType = AudioFileType::AAC;
                }
                else {
                    this->maybeType = AudioFileType::Movie;
                }
            }
        }

        return *this->maybeType;
    }
    DecimalNumber offsetToAddToMarkerPositionsForRekordboxInSeconds() const override
    {
        // -- rekordbox and the CDJs have a bug in their ACC M4A code that causes about 50ms of silence to be
        // -- inserted a the beginning of a track. Because of this we need to offset the markers accordingly
        // -- so they still match other programs.
        return p_mp4OffsetToAddToMarkerPositionsForRekordboxInSeconds;
    }

    String releaseDate() const override
    {
        return MP4TrackFileInternal::stringValueForItemNamedInTag(mp4YearItemName, *this->mp4Tag);
    }
    void setReleaseDate(const String& releaseDate) override
    {
        String year;
        auto components = releaseDate.splitBySeparator('-');
        if (components.length()) {
            year = components.firstObject();
        }

        MP4TrackFileInternal::setStringValueForItemNamedInTag(year, mp4YearItemName, *this->mp4Tag);
    }

    boolean hasKey() const override
    {
        return true;
    }
    String key() const override
    {
        return MP4TrackFileInternal::stringValueForItemNamedInTag(mp4KeyItemName, *this->mp4Tag);
    }
    void setKey(const String& musicalKey) override
    {
        MP4TrackFileInternal::setStringValueForItemNamedInTag(musicalKey, mp4KeyItemName, *this->mp4Tag);
    }

    String composer() const override
    {
        return MP4TrackFileInternal::stringValueForItemNamedInTag(mp4ComposerItemName, *this->mp4Tag);
    }
    void setComposer(const String& composer) override
    {
        MP4TrackFileInternal::setStringValueForItemNamedInTag(composer, mp4ComposerItemName, *this->mp4Tag);
    }

    String grouping() const override
    {
        return MP4TrackFileInternal::stringValueForItemNamedInTag(mp4GroupingItemName, *this->mp4Tag);
    }
    void setGrouping(const String& grouping) override
    {
        MP4TrackFileInternal::setStringValueForItemNamedInTag(grouping, mp4GroupingItemName, *this->mp4Tag);
    }

    String bpm() const override
    {
        return String::stringWithFormat("%d", MP4TrackFileInternal::integerValueForItemNamedInTag(mp4BpmItemName, *this->mp4Tag));
    }
    void setBpm(const String& beatsPerMinute) override
    {
        MP4TrackFileInternal::setIntegerValueForItemNamedInTag(beatsPerMinute.integerValue(), mp4BpmItemName, *this->mp4Tag);
    }

    boolean hasRecordLabel() const override
    {
        return true;
    }
    String recordLabel() const override
    {
        return MP4TrackFileInternal::stringValueForItemNamedInTag(mp4LabelItemName, *this->mp4Tag);
    }
    void setRecordLabel(const String& recordLabel) override
    {
        MP4TrackFileInternal::setStringValueForItemNamedInTag(recordLabel, mp4LabelItemName, *this->mp4Tag);
        MP4TrackFileInternal::setStringValueForItemNamedInTag(recordLabel, mp4PublisherItemName, *this->mp4Tag);
    }

    Optional<uinteger> maybeRating() const override
    {
        auto ratingAsString = MP4TrackFileInternal::stringValueForItemNamedInTag(mp4RatingItemName, *this->mp4Tag);
        if (!ratingAsString.length()) {
            return nothing;
        }

        return ratingAsString.integerValue();
    }
    void setRating(const Optional<uinteger>& maybeRating) override
    {
        MP4TrackFileInternal::setStringValueForItemNamedInTag(maybeRating.isValid() ?
                                                              String::stringWithFormat("%d", *maybeRating) : String{ },
                                                              mp4RatingItemName,
                                                              *this->mp4Tag);
    }

    String tags(void) const override
    {
        return MP4TrackFileInternal::stringValueForItemNamedInTag(mp4RekordBuddyTagsItemName, *this->mp4Tag);
    }
    void setTags(const String& tags) override
    {
        MP4TrackFileInternal::setStringValueForItemNamedInTag(tags, mp4RekordBuddyTagsItemName, *this->mp4Tag);
    }

    Blob artwork() const override
    {
        auto item = this->mp4Tag->item(mp4ArtworkItemName);
        if (!item.isValid()) {
            return { };
        }

        auto coverArtList = item.toCoverArtList();
        auto coverArt = coverArtList.front();
        auto coverArtData = coverArt.data();
        auto size = coverArtData.size();
        if (!size) {
            return { };
        }

        return Blob::withMemoryAndSize(reinterpret_cast<const byte*>(coverArtData.data()), size);
    }
    void setArtwork(const Blob& artwork) override
    {
        if (!artwork.size()) {
            this->mp4Tag->removeItem(mp4ArtworkItemName);
            return;
        }

        TagLib::ByteVector data(reinterpret_cast<const char*>(artwork.data().get()), static_cast<unsigned int >(artwork.size()));
        TagLib::MP4::CoverArt newCoverArt(TagLib::MP4::CoverArt::Unknown, data);
        TagLib::MP4::CoverArtList newCoverArtList;
        newCoverArtList.append(newCoverArt);

        TagLib::MP4::Item newItem(newCoverArtList);
        // -- TODO: This needs to be set to the correct type.
        newItem.setAtomDataType(TagLib::MP4::AtomDataType::TypePNG);

        this->mp4Tag->setItem(mp4ArtworkItemName, newItem);
    }

    boolean seratoBeatGridIsLocked() const override
    {
        return TrackFileInternal::seratoBeatGridLockFlagFromMarkerV2Data(MP4TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->mp4Tag));
    }
    void setSeratoBeatGridIsLocked(boolean seratoBeatGridIsLocked) override
    {
        auto decodedData = MutableBlob::withBase64String(MP4TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->mp4Tag));
        TrackFileInternal::setSeratoBeatGridLockFlagInDecodedMarkerV2Data(seratoBeatGridIsLocked, decodedData);

        auto base64Blob = Blob::withStringWithoutTerminator(decodedData.base64String());
        if (base64Blob.size()) {
            constexpr integer paddingMultipleOf = 256;
            count size = base64Blob.size();
            count paddingSize = (((size + paddingMultipleOf - 1) / paddingMultipleOf) * paddingMultipleOf) - size;
            if (paddingSize) {
                MutableBlob mutableBlob(base64Blob);
                mutableBlob.append(MutableBlob::withCapacity(paddingSize));
                base64Blob = Blob(std::move(mutableBlob));
            }
        }

        this->replaceFrameNamedWithDataAndVersion(mp4SeratoMarkersV2ItemName, mp4SeratoMarkersV2FrameDescription, base64Blob, 1, 1);
    }

    Array<SeratoMarker::OfSomeSort> seratoMarkers() const override
    {
        return MP4TrackFileInternal::seratoMarkersInTag(*this->mp4Tag);
    }
    void setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers) override
    {
        auto decodedData = MutableBlob::withBase64String(MP4TrackFileInternal::seratoMarkersV2Base64StringInTag(*this->mp4Tag));

        this->replaceFrameNamedWithDataAndVersion(mp4SeratoMarkersV1ItemName, mp4SeratoMarkersV1FrameDescription,
                                                  TrackFileInternal::rawSeratoMarkersV1BlobFrom(seratoMarkers), 2, 5);
        this->replaceFrameNamedWithDataAndVersion(mp4SeratoBeatgridItemName, mp4SeratoBeatgridFrameDescription,
                                                  SeratoGridMarker::gridMarkerDataFrom(seratoMarkers), 1, 0);
        decodedData = TrackFileInternal::seratoMarkersV2DecodedDataFromAndOldMarkersV2DecodedData(seratoMarkers, decodedData);

        auto base64Blob = Blob::withStringWithoutTerminator(decodedData.base64String());
        if (base64Blob.size()) {
            constexpr integer paddingMultipleOf = 256;
            count size = base64Blob.size();
            count paddingSize = (((size + paddingMultipleOf - 1) / paddingMultipleOf) * paddingMultipleOf) - size;
            if (paddingSize) {
                MutableBlob mutableBlob(base64Blob);
                mutableBlob.append(MutableBlob::withCapacity(paddingSize));
                base64Blob = Blob(std::move(mutableBlob));
            }
        }

        this->replaceFrameNamedWithDataAndVersion(mp4SeratoMarkersV2ItemName, mp4SeratoMarkersV2FrameDescription, base64Blob, 1, 1);
    }
};

}
