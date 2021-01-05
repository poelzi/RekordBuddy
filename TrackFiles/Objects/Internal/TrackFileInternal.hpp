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

#include <TrackFiles/TrackFile.hpp>
#include <TrackFiles/SeratoMarkers/SeratoCueMarker.hpp>
#include <TrackFiles/SeratoMarkers/SeratoLoopMarker.hpp>
#include <TrackFiles/SeratoMarkers/SeratoGridMarker.hpp>

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include <tag/tfile.h>
#include <tag/tpropertymap.h>
#include <tag/audioproperties.h>
#include <tag/tbytevectorstream.h>
#include <tag/tfilestream.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace NxA {

#define NXA_OBJECT_CLASS                                    TrackFile
#define NXA_INTERNAL_OBJECT_IS_PURE_VIRTUAL
#define NXA_INTERNAL_OBJECT_DOES_NOT_NEED_EQUAL_OPERATOR

// -- Internal Interface
class TrackFileInternal
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Types
    struct SeratoMarkerV2HeaderStruct {
        byte majorVersion;
        byte minorVersion;
        byte data[0];
    };

    struct SeratoMarkerV1HeaderStruct {
        byte markerCount[4];
        byte data[0];
    };

    struct SeratoMarkerV1FooterStruct {
        byte footer[4];
    };

    struct SeratoBpmLockTagStruct {
        byte tag[8];
        byte size[4];
        byte locked;
    };

    // -- Class Methods
    static NotNull<const byte*> nextTagPositionAfterTag(NotNull<const byte*> currentTagPosition)
    {
        while(*currentTagPosition) {
            currentTagPosition = currentTagPosition + 1;
        }

        auto tagSize = Platform::bigEndianUInteger32ValueAt(currentTagPosition + 1);
        currentTagPosition = currentTagPosition + 5 + tagSize;

        return currentTagPosition;
    }

    static bool isAValidTagAt(NotNull<const byte*> currentTagPosition)
    {
        auto firstByte = *currentTagPosition;

        // -- Some Serato files can be corrupted and seem to have corrupted data after their last CUE tag.
        // -- For example 43 55 45 00 00 00 00 15 00 07 00 03 6F 05 00 88 00 CC 00 00 45 6E 65 72 67 79 20 38 00 01 01 00 42 50 4D 4C 4F 43 4B
        // -- which is CUE.................Energy 8**.BPMLOK whereas the two * bytes are not a valid tag name but are placed where one is expected.
        // -- This attemps to detect those so we can continue on to the next valid tag.
        return !((firstByte < 'A') || (firstByte > 'Z'));
    }

    static Blob seratoMarkerV2TagDataFrom(NotNull<const byte*> tagStart)
    {
        String tagName(reinterpret_cast<const char*>(tagStart.get()));
        count sizeOfNameField = tagName.length() + 1;
        constexpr count sizeOfSizeField = 4;

        auto sizePosition = tagStart + sizeOfNameField;
        count tagSize = Platform::bigEndianUInteger32ValueAt(sizePosition) + sizeOfNameField + sizeOfSizeField;
        return Blob::withMemoryAndSize(tagStart, tagSize);
    }

    static Blob nonSeratoMarkerTagsFromDecodedData(const MutableBlob& decodedData)
    {
        if (!decodedData.size()) {
            return { };
        }

        auto seratoMarkerStruct = decodedData.data().forceAs<const SeratoMarkerV2HeaderStruct*>();
        if ((seratoMarkerStruct->majorVersion != 1) || (seratoMarkerStruct->minorVersion != 1)) {
            return { };
        }

        MutableBlob result;

        auto markerDataEnd = NotNull<const byte*>{seratoMarkerStruct.forceAs<const byte*>() + decodedData.size()};
        auto tagStart = NotNull<const byte*>{static_cast<const byte*>(seratoMarkerStruct->data)};

        while ((tagStart < markerDataEnd) && *tagStart) {
            if (!Internal::isAValidTagAt(tagStart)) {
                while ((tagStart < markerDataEnd) && *tagStart++) { }
                continue;
            }
            else if (!::strcmp(reinterpret_cast<const character*>(tagStart.get()), "CUE") || !::strcmp(reinterpret_cast<const character*>(tagStart.get()), "LOOP")) {
                tagStart = Internal::nextTagPositionAfterTag(tagStart);
                continue;
            }

            result.append(Internal::seratoMarkerV2TagDataFrom(tagStart));

            tagStart = Internal::nextTagPositionAfterTag(tagStart);
        }

        return std::move(result);
    }

    static void addSeratoMarkersV1FromRawByteArrayTo(const byte* seratoMarkerData,
                                                     count totalSize,
                                                     MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers)
    {
        if(totalSize != sizeof(SeratoMarkerV1HeaderStruct) + SeratoCueMarker::sizeOfV1RawMarker() * 5 + SeratoLoopMarker::sizeOfV1RawMarker() * 9 + sizeof(SeratoMarkerV1FooterStruct)) {
            // -- Invalid raw V1 Serato marker data
            return;
        }

        auto seratoMarkerPos = seratoMarkerData + sizeof(SeratoMarkerV1HeaderStruct);

        for (int i = 0; i < 5; ++i) {
            if (SeratoCueMarker::isValidV1RawMarker(seratoMarkerPos)) {
                auto maybeMarker = SeratoCueMarker::maybeMarkerV1WithIndexAndRawMemoryAt(i, seratoMarkerPos);
                if (maybeMarker.isValid()) {
                    seratoMarkers.emplaceAppend(std::move(*maybeMarker));
                }
            }

            seratoMarkerPos += SeratoCueMarker::sizeOfV1RawMarker();
        }

        for (int i = 0; i < 9; ++i) {
            if (SeratoLoopMarker::isValidV1RawMarker(seratoMarkerPos)) {
                auto maybeMarker = SeratoLoopMarker::maybeMarkerV1WithIndexAndRawMemoryAt(i, seratoMarkerPos);
                if (maybeMarker.isValid()) {
                    seratoMarkers.emplaceAppend(std::move(*maybeMarker));
                }
            }

            seratoMarkerPos += SeratoLoopMarker::sizeOfV1RawMarker();
        }
    }

    static void addSeratoMarkersV1FromEncodedByteArrayTo(const byte* markerData,
                                                         count totalSize,
                                                         MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers)
    {
        if (totalSize != sizeof(SeratoMarkerV1HeaderStruct) + (SeratoCueMarker::sizeOfV1EncodedMarker() * 5) + (SeratoLoopMarker::sizeOfV1EncodedMarker() * 9) + sizeof(SeratoMarkerV1FooterStruct)) {
            // -- Invalid encoded V1 Serato marker data
            return;
        }

        auto seratoMarkerPos = markerData + sizeof(SeratoMarkerV1HeaderStruct);

        for (int i = 0; i < 5; ++i) {
            if (SeratoCueMarker::isValidV1EncodedMarker(seratoMarkerPos)) {
                auto maybeMarker = SeratoCueMarker::maybeMarkerV1WithIndexAndEncodedMemoryAt(i, seratoMarkerPos);
                if (maybeMarker.isValid()) {
                    seratoMarkers.emplaceAppend(std::move(*maybeMarker));
                }
            }

            seratoMarkerPos += SeratoCueMarker::sizeOfV1EncodedMarker();
        }

        for (int i = 0; i < 9; ++i) {
            if (SeratoLoopMarker::isValidV1EncodedMarker(seratoMarkerPos)) {
                auto maybeMarker = SeratoLoopMarker::maybeMarkerV1WithIndexAndEncodedMemoryAt(i, seratoMarkerPos);
                if (maybeMarker.isValid()) {
                    seratoMarkers.emplaceAppend(std::move(*maybeMarker));
                }
            }

            seratoMarkerPos += SeratoLoopMarker::sizeOfV1EncodedMarker();
        }
    }

    static void addSeratoMarkersV2FromBase64StringTo(const String& base64String, MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers)
    {
        if (base64String.isEmpty()) {
            return;
        }

        auto decodedData = Blob::withBase64String(base64String);
        auto markerStruct = decodedData.data().forceAs<const SeratoMarkerV2HeaderStruct*>();
        if ((markerStruct->majorVersion != 1) || (markerStruct->minorVersion != 1)) {
            return;
        }

        auto markerDataEnd = markerStruct.forceAs<const byte*>() + decodedData.size();
        auto tagStart = NotNull<const byte*>{ static_cast<const byte*>(markerStruct->data) };

        while ((tagStart < markerDataEnd) && *tagStart) {
            if (!Internal::isAValidTagAt(tagStart)) {
                while ((tagStart < markerDataEnd) && *tagStart++) { }
                continue;
            }
            else if (!::strcmp(reinterpret_cast<const character*>(tagStart.get()), "CUE")) {
                auto maybeMarker = SeratoCueMarker::maybeMarkerWithMemoryAt(tagStart);
                if (maybeMarker.isValid()) {
                    seratoMarkers.emplaceAppend(std::move(*maybeMarker));
                }
            }
            else if (!::strcmp(reinterpret_cast<const character*>(tagStart.get()), "LOOP")) {
                auto maybeMarker = SeratoLoopMarker::maybeMarkerWithMemoryAt(tagStart);
                if (maybeMarker.isValid()) {
                    seratoMarkers.emplaceAppend(std::move(*maybeMarker));
                }
            }

            tagStart = Internal::nextTagPositionAfterTag(tagStart);
        }
    }

    static void addSeratoGridMarkersFromTo(const byte* gridMarkerData, count size, MutableArray<SeratoMarker::OfSomeSort>& markers)
    {
        SeratoGridMarker::addMarkersWithMemoryAtTo(gridMarkerData, size, markers);
    }

    static Blob rawSeratoMarkersV1BlobFrom(const MutableArray<SeratoMarker::OfSomeSort>& markers)
    {
        MutableBlob blobData;

        SeratoMarkerV1HeaderStruct markersHeader;
        markersHeader.markerCount[0] = 0;
        markersHeader.markerCount[1] = 0;
        markersHeader.markerCount[2] = 0;
        markersHeader.markerCount[3] = 14;
        blobData.appendMemoryWithSize(reinterpret_cast<byte*>(&markersHeader), sizeof(SeratoMarkerV1HeaderStruct));

        for (int i = 0; i < 5; ++i) {
            // Find the cue with this index
            bool cueFound = false;

            for (auto&& marker : markers) {
                if (!marker.isType<SeratoCueMarker>()) {
                    continue;
                }

                auto& seratoCueMarker = marker.get<SeratoCueMarker>();
                if (seratoCueMarker.index() != i) {
                    continue;
                }

                seratoCueMarker.addRawMarkerV1TagTo(blobData);
                cueFound = true;
                break;
            }

            // Otherwise, write an empty cue
            if (!cueFound) {
                SeratoCueMarker::addEmptyRawMarkerV1TagTo(blobData);
            }
        }

        for (int i = 0; i < 9; ++i) {
            // Find the loop with this index
            bool loopFound = false;
            for (auto&& marker : markers) {
                if (!marker.isType<SeratoLoopMarker>()) {
                    continue;
                }

                auto& seratoLoopMarker = marker.get<SeratoLoopMarker>();
                if (seratoLoopMarker.index() != i) {
                    continue;
                }

                seratoLoopMarker.addRawMarkerV1TagTo(blobData);
                loopFound = true;
                break;
            }

            // Otherwise, write an empty loop
            if (!loopFound) {
                SeratoLoopMarker::addEmptyRawMarkerV1TagTo(blobData);
            }
        }

        // -- This marks the end of tags.
        blobData.append(0x00);
        blobData.append(0xFF);
        blobData.append(0xFF);
        blobData.append(0xFF);

        return std::move(blobData);
    }

    static Blob markersV1Id3EncodedBlobFrom(const MutableArray<SeratoMarker::OfSomeSort>& markers)
    {
        MutableBlob blobData;

        SeratoMarkerV1HeaderStruct markersHeader;
        markersHeader.markerCount[0] = 0;
        markersHeader.markerCount[1] = 0;
        markersHeader.markerCount[2] = 0;
        markersHeader.markerCount[3] = 14;
        blobData.appendMemoryWithSize(reinterpret_cast<byte*>(&markersHeader), sizeof(SeratoMarkerV1HeaderStruct));

        for (int i = 0; i < 5; ++i) {
            // Find the cue with this index
            bool cueFound = false;
            for (auto&& marker : markers) {
                if (!marker.isType<SeratoCueMarker>()) {
                    continue;
                }

                auto& seratoCueMarker = marker.get<SeratoCueMarker>();
                if (seratoCueMarker.index() == i) {
                    seratoCueMarker.addEncodedMarkerV1TagTo(blobData);
                    cueFound = true;
                    break;
                }
            }

            // Otherwise, write an empty cue
            if (!cueFound) {
                SeratoCueMarker::addEmptyEncodedMarkerV1TagTo(blobData);
            }
        }

        for (int i = 0; i < 9; ++i) {
            // Find the loop with this index
            bool loopFound = false;
            for (auto&& marker : markers) {
                if (!marker.isType<SeratoLoopMarker>()) {
                    continue;
                }

                auto& seratoLoopMarker = marker.get<SeratoLoopMarker>();
                if (seratoLoopMarker.index() == i) {
                    seratoLoopMarker.addEncodedMarkerV1TagTo(blobData);
                    loopFound = true;
                    break;
                }
            }

            // Otherwise, write an empty loop
            if (!loopFound) {
                SeratoLoopMarker::addEmptyEncodedMarkerV1TagTo(blobData);
            }
        }

        // -- This marks the end of tags.
        blobData.append(0x07);
        blobData.append(0x7F);
        blobData.append(0x7F);
        blobData.append(0x7F);

        return std::move(blobData);
    }

    static MutableBlob seratoMarkersV2DecodedDataFromAndOldMarkersV2DecodedData(const MutableArray<SeratoMarker::OfSomeSort>& markers,
                                                                                const MutableBlob& decodedData)
    {
        if (!markers.length()) {
            return { };
        }

        MutableBlob newDecodedData;

        SeratoMarkerV2HeaderStruct markersHeader;
        markersHeader.majorVersion = 1;
        markersHeader.minorVersion = 1;
        newDecodedData.appendMemoryWithSize(reinterpret_cast<byte*>(&markersHeader), sizeof(SeratoMarkerV2HeaderStruct));

        for (auto&& marker : markers) {
            if (marker.isType<SeratoGridMarker>()) {
                continue;
            }

            withVariant(marker, [&newDecodedData](auto&& marker) {
                marker.addMarkerV2TagTo(newDecodedData);
            });
        }

        newDecodedData.append(Internal::nonSeratoMarkerTagsFromDecodedData(decodedData));

        // -- This marks the end of tags.
        newDecodedData.append('\0');

        return newDecodedData;
    }

    static boolean seratoBeatGridLockFlagFromMarkerV2Data(const String& base64String)
    {
        if (base64String.isEmpty()) {
            return false;
        }

        auto decodedData = Blob::withBase64String(base64String);
        auto markerStruct = decodedData.data().forceAs<const SeratoMarkerV2HeaderStruct*>();
        if ((markerStruct->majorVersion != 1) || (markerStruct->minorVersion != 1)) {
            return false;
        }

        auto markerDataEnd{markerStruct.forceAs<const byte*>() + decodedData.size()};
        NotNull<const byte*> tagStart{static_cast<const byte*>(markerStruct->data)};

        while ((tagStart < markerDataEnd) && *tagStart) {
            if (!Internal::isAValidTagAt(tagStart)) {
                while ((tagStart < markerDataEnd) && *tagStart++) { }
                continue;
            }
            else if (::strcmp(reinterpret_cast<const character*>(tagStart.get()), "BPMLOCK")) {
                tagStart = Internal::nextTagPositionAfterTag(tagStart);
                continue;
            }

            auto bpmLockData = tagStart.forceAs<const SeratoBpmLockTagStruct*>();
            return (bpmLockData->locked != 0);
        }

        return false;
    }

    static void setSeratoBeatGridLockFlagInDecodedMarkerV2Data(boolean beatGridLockFlag, MutableBlob& decodedData)
    {
        if (decodedData.size()) {
            auto markerStruct = decodedData.data().forceAs<const SeratoMarkerV2HeaderStruct*>();
            if ((markerStruct->majorVersion != 1) || (markerStruct->minorVersion != 1)) {
                return;
            }

            NotNull<const byte*> markerDataEnd{markerStruct.forceAs<const byte*>() + decodedData.size()};
            NotNull<const byte*> tagStart{reinterpret_cast<const byte*>(markerStruct->data)};

            while ((tagStart < markerDataEnd) && *tagStart) {
                if (!Internal::isAValidTagAt(tagStart)) {
                    while ((tagStart < markerDataEnd) && *tagStart++) { }
                    continue;
                }
                else if (::strcmp(reinterpret_cast<const character*>(tagStart.get()), "BPMLOCK")) {
                    tagStart = Internal::nextTagPositionAfterTag(tagStart);
                    continue;
                }

                auto bpmLockData = tagStart.forceAs<const SeratoBpmLockTagStruct *>().constCast<SeratoBpmLockTagStruct*>();
                if (bpmLockData->locked == beatGridLockFlag) {
                    return;
                }

                bpmLockData->locked = beatGridLockFlag;
                return;
            }
        }
        else {
            SeratoMarkerV2HeaderStruct header;
            header.majorVersion = 1;
            header.minorVersion = 1;

            decodedData.append(Blob::withMemoryAndSize(reinterpret_cast<const byte*>(&header), sizeof(SeratoMarkerV2HeaderStruct)));
        }

        SeratoBpmLockTagStruct lockStruct;

        memcpy(lockStruct.tag, "BPMLOCK", 8);
        Platform::writeBigEndianUInteger32ValueAt(1, lockStruct.size);
        lockStruct.locked = beatGridLockFlag;

        decodedData.append(Blob::withMemoryAndSize(reinterpret_cast<const byte*>(&lockStruct), sizeof(SeratoBpmLockTagStruct)));
    }

    static String roundedBpmValueFromValue(const String& bpm)
    {
        auto asInteger = bpm.decimalValue().asInteger();
        if (asInteger) {
            return String::stringWithFormat("%d", asInteger);
        }
        else {
            return { };
        }
    }

    // -- Instance Variables
    FilePath filePath;

    mutable Optional<count> maybeFileSize;

    mutable std::unique_ptr<TagLib::FileStream> fileStream;
    mutable TagLib::File* file;
    mutable TagLib::Tag* tag;
    mutable TagLib::AudioProperties* audioProperties;

    // -- Constructor & Destructors
    TrackFileInternal(const FilePath& path) : filePath(path) { }
    virtual ~TrackFileInternal() = default;

    // -- Operators
    virtual bool operator==(const TrackFileInternal& other) const noexcept
    {
        return this->filePath == other.filePath;
    }
    virtual bool operator<(const TrackFileInternal& other) const noexcept
    {
        return this->filePath < other.filePath;
    }

    // -- Instance Methods
    virtual void load() const
    {
        if (!this->fileStream) {
            auto fp = this->filePath.asPlatformNativeString();
            this->fileStream = std::make_unique<TagLib::FileStream>(fp.c_str());
        }

        this->fileStream->clear();
        this->fileStream->seek(0);
    }
    virtual boolean parse() const
    {
        this->load();

        this->tag = this->file->tag();
        if (!this->tag) {
            return false;
        }

        this->audioProperties = this->file->audioProperties();
        if (!this->audioProperties) {
            return false;
        }

        return true;
    }
    virtual boolean save() const
    {
        NXA_ASSERT_NOT_NULL(this->file);

        // -- This is misleading. It doesn't actually save anything to disk.
        // -- Instead, real saving takes place in the file's destructor. #ugh
        return this->file->save();
    }

    count sizeInBytes() const
    {
        if (!this->maybeFileSize.isValid()) {
            this->maybeFileSize = File::sizeOfFileAt(this->filePath);
        }

        return *this->maybeFileSize;
    }
    virtual AudioFileType type() const = 0;
    virtual DecimalNumber offsetToAddToMarkerPositionsForRekordboxInSeconds() const
    {
        return { };
    }

    String title() const
    {
        auto title = String(this->tag->title().to8Bit(true));
        if (title.hasNonPrintableCharacters()) {
            return String::stringByFilteringNonPrintableCharactersIn(title);
        }

        return title;
    }
    void setTitle(const String& title)
    {
        this->tag->setTitle(TagLib::String(title.asUTF8(), TagLib::String::UTF8));
    }

    String artist() const
    {
        auto artist = String(this->tag->artist().to8Bit(true));
        if (artist.hasNonPrintableCharacters()) {
            return String::stringByFilteringNonPrintableCharactersIn(artist);
        }

        return artist;
    }
    void setArtist(const String& artist)
    {
        this->tag->setArtist(TagLib::String(artist.asUTF8(), TagLib::String::UTF8));
    }

    String genre() const
    {
        auto genre = String(this->tag->genre().to8Bit(true));
        if (genre.hasNonPrintableCharacters()) {
            return String::stringByFilteringNonPrintableCharactersIn(genre);
        }

        return genre;
    }
    void setGenre(const String& genre)
    {
        this->tag->setGenre(TagLib::String(genre.asUTF8(), TagLib::String::UTF8));
    }

    String comments() const
    {
        auto comments = String(this->tag->comment().to8Bit(true));
        if (comments.hasNonPrintableCharacters()) {
            return String::stringByFilteringNonPrintableCharactersIn(comments);
        }

        return comments;
    }
    void setComments(const String& comments)
    {
        this->tag->setComment(TagLib::String(comments.asUTF8(), TagLib::String::UTF8));
    }

    String album() const
    {
        auto album = String(this->tag->album().to8Bit(true));
        if (album.hasNonPrintableCharacters()) {
            return String::stringByFilteringNonPrintableCharactersIn(album);
        }

        return album;
    }
    void setAlbum(const String& album)
    {
        this->tag->setAlbum(TagLib::String(album.asUTF8(), TagLib::String::UTF8));
    }

    count trackNumber() const
    {
        return this->tag->track();
    }
    void setTrackNumber(count trackNumber)
    {
        this->tag->setTrack(static_cast<unsigned int>(trackNumber));
    }

    virtual String releaseDate() const = 0;
    virtual void setReleaseDate(const String&) = 0;

    virtual boolean hasKey() const
    {
        return false;
    }
    virtual String key() const
    {
        NXA_ALOG("Invalid base class call.");
    }
    virtual void setKey(const String&)
    {
        NXA_ALOG("Invalid base class call.");
    }

    virtual String composer() const = 0;
    virtual void setComposer(const String&) = 0;

    virtual String grouping() const = 0;
    virtual void setGrouping(const String&) = 0;

    virtual String bpm() const = 0;
    virtual void setBpm(const String&) = 0;

    virtual boolean hasRecordLabel() const
    {
        return false;
    }
    virtual String recordLabel() const
    {
        NXA_ALOG("Invalid base class call.");
    }
    virtual void setRecordLabel(const String&)
    {
        NXA_ALOG("Invalid base class call.");
    }

    virtual boolean hasRemixer() const
    {
        return false;
    }
    virtual String remixer() const
    {
        NXA_ALOG("Invalid base class call.");
    }
    virtual void setRemixer(const String&)
    {
        NXA_ALOG("Invalid base class call.");
    }

    virtual Optional<uinteger> maybeRating() const = 0;
    virtual void setRating(const Optional<uinteger>&) = 0;

    virtual String tags(void) const = 0;
    virtual void setTags(const String&) = 0;

    virtual Blob artwork() const = 0;
    virtual void setArtwork(const Blob&) = 0;

    count audioDataSizeInBytes() const
    {
        return (static_cast<uinteger64>(this->lengthInMilliseconds()) *
                static_cast<uinteger64>(this->bitRateInKiloBitsPerSecond()) * 1024) / 8 / 1000;
    }
    count lengthInMilliseconds() const
    {
        return this->audioProperties->lengthInMilliseconds();
    }
    count bitRateInKiloBitsPerSecond() const
    {
        return this->audioProperties->bitrate();
    }
    virtual boolean hasBitDepth() const
    {
        return false;
    }
    virtual count bitDepthInBits() const
    {
        NXA_ALOG("Invalid base class call.");
    }
    count sampleRateInSamplesPerSecond() const
    {
        return this->audioProperties->sampleRate();
    }

    virtual boolean seratoBeatGridIsLocked() const = 0;
    virtual void setSeratoBeatGridIsLocked(boolean) = 0;

    virtual Array<SeratoMarker::OfSomeSort> seratoMarkers() const = 0;
    virtual void setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>&) = 0;
};

}
