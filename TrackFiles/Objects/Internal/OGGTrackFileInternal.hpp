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

#include <TrackFiles/OGGTrackFile.hpp>
#include <TrackFiles/SeratoMarkers/SeratoGridMarker.hpp>

#include "Internal/TrackFileInternal.hpp"

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#include <tag/xiphcomment.h>
#include <tag/vorbisfile.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace NxA {

#define NXA_OBJECT_CLASS                                    OGGTrackFile
#define NXA_OBJECT_INTERNAL_BASE_CLASS                      TrackFileInternal

// -- Internal Interface
class OGGTrackFileInternal : public NXA_OBJECT_INTERNAL_BASE_CLASS
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Types
    struct OGGSeratoMarkersHeaderStruct
    {
        byte mimeType[25];
        byte filename[1];
        byte description[16];
        byte majorVersion;
        byte minorVersion;
        byte data[0];
    };

    // -- Constants
    static constexpr auto oggComposerFieldName = "COMPOSER";
    static constexpr auto oggGroupingFieldName = "GROUPING";
    static constexpr auto oggBpmFieldName = "BPM";
    static constexpr auto oggRecordLabelFieldName = "LABEL";
    static constexpr auto oggRemixerFieldName = "REMIXER";
    static constexpr auto oggDateFieldName = "DATE";
    static constexpr auto oggYearFieldName = "YEAR";
    static constexpr auto oggKeyFieldName = "INITIALKEY";
    static constexpr auto oggRatingFieldName = "RATING WMP";
    static constexpr auto oggRekordbuddyTagsFieldName = "REKORDBUDDYTAGS";

    static constexpr auto oggSeratoMarkersItemName = "SERATO_MARKERS";
    static constexpr auto oggSeratoMarkersV2ItemName = "SERATO_MARKERS2";
    static constexpr auto oggSeratoBeatgridItemName = "SERATO_BEATGRID";

    // -- Class Methods
    static String stringValueForFieldNamedInComment(const character* name, const TagLib::Ogg::XiphComment& xiphComment)
    {
        auto& stringList = xiphComment.fieldListMap()[name];
        if (stringList.size() == 0) {
            return { };
        }

        auto value = String(stringList.front().to8Bit(true).c_str());
        if (value.hasNonPrintableCharacters()) {
            value = String::stringByFilteringNonPrintableCharactersIn(value);
        }
        return value;
    }

    static void setStringValueForFieldNamedInComment(const String& value, const character* name, TagLib::Ogg::XiphComment& xiphComment)
    {
        xiphComment.addField(name, TagLib::String(value.asUTF8(), TagLib::String::UTF8));
    }

    static boolean removingFieldNamedInCommentDidRemoveSomething(const String& name, TagLib::Ogg::XiphComment& xiphComment)
    {
        auto key = TagLib::String(name.asUTF8(), TagLib::String::UTF8);
        auto map = xiphComment.fieldListMap();
        auto pos = map.find(key);
        if (pos == map.end()) {
            return false;
        }

        map.erase(pos);
        return true;
    }

    static String releaseDateInComment(const TagLib::Ogg::XiphComment& xiphComment)
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggDateFieldName, xiphComment);
    }

    static void setReleaseDateInComment(const String& date, TagLib::Ogg::XiphComment& xiphComment)
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(date, oggDateFieldName, xiphComment);

        auto components = date.splitBySeparator('-');
        if (components.length()) {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(components.firstObject(), oggYearFieldName, xiphComment);
        }
        else {
            OGGTrackFileInternal::setStringValueForFieldNamedInComment(String(""), oggYearFieldName, xiphComment);
        }
    }

    static void replaceFieldInCommentWith(const character* fieldName, TagLib::Ogg::XiphComment& xiphComment, const String& base64String)
    {
        xiphComment.removeFields(fieldName);

        if (base64String.isEmpty()) {
            return;
        }

        xiphComment.addField(oggSeratoMarkersV2ItemName, TagLib::String(base64String.asUTF8(), TagLib::String::UTF8));
    }

    static String seratoMarkersV2Base64StringInComment(const TagLib::Ogg::XiphComment& xiphComment)
    {
        auto& fieldListMap = xiphComment.fieldListMap();

        auto encodedData = fieldListMap[oggSeratoMarkersV2ItemName].toString();
        auto encodedDataSize = encodedData.size();
        if (!encodedDataSize) {
            return { };
        }

        return { encodedData.data(TagLib::String::UTF8).data(), encodedDataSize };
    }

    static Array<SeratoMarker::OfSomeSort> markersInComment(const TagLib::Ogg::XiphComment& xiphComment)
    {
        MutableArray<SeratoMarker::OfSomeSort> markers;

        TrackFileInternal::addSeratoMarkersV2FromBase64StringTo(OGGTrackFileInternal::seratoMarkersV2Base64StringInComment(xiphComment), markers);

        auto& fieldListMap = xiphComment.fieldListMap();
        auto beatGridEncodedData = fieldListMap[oggSeratoBeatgridItemName].toString();
        auto encodedBeatGridDataSize = beatGridEncodedData.size();
        if (encodedBeatGridDataSize) {
            auto majorVersion = beatGridEncodedData.substr(0, 8).toInt();
            auto minorVersion = beatGridEncodedData.substr(8, 8).toInt();
            if ((majorVersion == 1) && (minorVersion == 0)) {
                auto numberOfGridMarkers = beatGridEncodedData.substr(16, 8).toInt();
                if (numberOfGridMarkers) {
                    auto markerStrings = beatGridEncodedData.substr(25).split("(");
                    if (static_cast<integer64>(markerStrings.size()) == numberOfGridMarkers) {
                        for (auto&& markerString : markerStrings) {
                            auto values = markerString.split(",");
                            auto bpm = values[1].substr(0, values[1].length() - 1);

                            markers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(DecimalNumber(values[0].to8Bit(true).c_str()), DecimalNumber(bpm.to8Bit(true).c_str())));
                        }
                    }
                }
            }
        }

        return std::move(markers);
    }

    // -- Instance Variables
    mutable std::unique_ptr<TagLib::Vorbis::File> vorbisFile;
    mutable TagLib::Ogg::XiphComment *xiphComment;

    // -- Constructor & Destructors
    OGGTrackFileInternal(const FilePath& path) : TrackFileInternal(path), xiphComment(nullptr) { }
    virtual ~OGGTrackFileInternal() = default;

    // -- Operators
    inline bool operator==(const OGGTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator==(other);
    }
    inline bool operator<(const OGGTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator<(other);
    }

    // -- Instance Methods
    void replaceGridMarkersFieldInCommentWith(const MutableArray<SeratoMarker::OfSomeSort>& markers) const
    {
        TagLib::Ogg::XiphComment& comment = *this->xiphComment;
        comment.removeFields(oggSeratoBeatgridItemName);

        if (!markers.length()) {
            return;
        }

        char buffer[32];

        TagLib::String markersString;
        count numberOfGridMarkersFound = 0;
        for (auto&& marker : markers) {
            if (!marker.isType<SeratoGridMarker>()) {
                continue;
            }

            auto& seratoGridMarker = marker.get<SeratoGridMarker>();
            ::snprintf(buffer, sizeof(buffer), "(%s000,%s0000)",
                       seratoGridMarker.positionInSecondsAsString().asUTF8(),
                       seratoGridMarker.beatsPerMinuteAsString().asUTF8());
            markersString.append(buffer);

            ++numberOfGridMarkersFound;
        }

        TagLib::String propertyString;
        propertyString.append("00000001");
        propertyString.append("00000000");

#if defined(NXA_PLATFORM_MACOS)
        ::snprintf(buffer, sizeof(buffer), "%08llu", numberOfGridMarkersFound);
#elif defined(NXA_PLATFORM_WINDOWS)
        ::snprintf(buffer, sizeof(buffer), "%08llu", numberOfGridMarkersFound);
#else
        #error Unsupported platform.
#endif

        propertyString.append(buffer);
        propertyString.append(markersString);

        comment.addField(oggSeratoBeatgridItemName, propertyString);
    }

    void removeFieldNamedIfAny(const String& name)
    {
        if (this->xiphComment == nullptr) {
            return;
        }

        Internal::removingFieldNamedInCommentDidRemoveSomething(name, *this->xiphComment);
    }

    // -- Overridden TrackFileInternal Instance Methods
    boolean parse() const override
    {
        this->load();

        this->vorbisFile = std::make_unique<TagLib::Vorbis::File>(this->fileStream.get(),
                                                                  true, TagLib::AudioProperties::ReadStyle::Fast);
        if (!this->vorbisFile->isValid()) {
            this->vorbisFile = nullptr;
            return false;
        }

        this->file = this->vorbisFile.get();

        this->xiphComment = this->vorbisFile->tag();
        if (!this->xiphComment) {
            this->vorbisFile = nullptr;
            return false;
        }

        return this->TrackFileInternal::parse();
    }
    boolean save() const override
    {
        if (!this->vorbisFile.get()) {
            return true;
        }

        return TrackFileInternal::save();
    }

    AudioFileType type() const override
    {
        return AudioFileType::OGG;
    }

    String releaseDate() const override
    {
        return OGGTrackFileInternal::releaseDateInComment(*this->xiphComment);
    }
    void setReleaseDate(const String& releaseDate) override
    {
        OGGTrackFileInternal::setReleaseDateInComment(releaseDate, *this->xiphComment);
    }

    boolean hasKey() const override
    {
        return true;
    }
    String key() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(OGGTrackFileInternal::oggKeyFieldName, *this->xiphComment);
    }
    void setKey(const String& musicalKey) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(musicalKey, OGGTrackFileInternal::oggKeyFieldName, *this->xiphComment);
    }

    String composer() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggComposerFieldName, *this->xiphComment);
    }
    void setComposer(const String& composer) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(composer, oggComposerFieldName, *this->xiphComment);
    }

    String grouping() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggGroupingFieldName, *this->xiphComment);
    }
    void setGrouping(const String& grouping) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(grouping, oggGroupingFieldName, *this->xiphComment);
    }

    String bpm() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggBpmFieldName, *this->xiphComment);
    }
    void setBpm(const String& beatsPerMinute) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(beatsPerMinute, oggBpmFieldName, *this->xiphComment);
    }

    boolean hasRecordLabel() const override
    {
        return true;
    }
    String recordLabel() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggRecordLabelFieldName, *this->xiphComment);
    }
    void setRecordLabel(const String& recordLabel) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(recordLabel, oggRecordLabelFieldName, *this->xiphComment);
    }

    boolean hasRemixer() const override
    {
        return true;
    }
    String remixer() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggRemixerFieldName, *this->xiphComment);
    }
    void setRemixer(const String& remixer) override
    {
        setStringValueForFieldNamedInComment(remixer, oggRemixerFieldName, *this->xiphComment);
    }

    Optional<uinteger> maybeRating() const override
    {
        auto ratingAsString = OGGTrackFileInternal::stringValueForFieldNamedInComment(oggRatingFieldName, *this->xiphComment);
        if (!ratingAsString.length()) {
            return nothing;
        }

        return ratingAsString.integerValue();
    }
    void setRating(const Optional<uinteger>& maybeRating) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(maybeRating.isValid() ?
                                                                   String::stringWithFormat("%d", *maybeRating) : String{ },
                                                                   oggRatingFieldName,
                                                                   *this->xiphComment);
    }

    String tags() const override
    {
        return OGGTrackFileInternal::stringValueForFieldNamedInComment(oggRekordbuddyTagsFieldName, *this->xiphComment);
    }
    void setTags(const String& tags) override
    {
        OGGTrackFileInternal::setStringValueForFieldNamedInComment(tags, oggRekordbuddyTagsFieldName, *this->xiphComment);
    }

    Blob artwork() const override
    {
        // -- TODO: Artwork to be implemented.
        return { };
    }
    void setArtwork(const Blob& artwork) override
    {
        // -- TODO: Artwork to be implemented.
    }

    boolean seratoBeatGridIsLocked() const override
    {
        return TrackFileInternal::seratoBeatGridLockFlagFromMarkerV2Data(OGGTrackFileInternal::seratoMarkersV2Base64StringInComment(*this->xiphComment));
    }
    void setSeratoBeatGridIsLocked(boolean seratoBeatGridIsLocked) override
    {
        auto decodedData = MutableBlob::withBase64String(OGGTrackFileInternal::seratoMarkersV2Base64StringInComment(*this->xiphComment));
        TrackFileInternal::setSeratoBeatGridLockFlagInDecodedMarkerV2Data(seratoBeatGridIsLocked, decodedData);
        OGGTrackFileInternal::replaceFieldInCommentWith(oggSeratoMarkersV2ItemName, *this->xiphComment, decodedData.base64String());
    }

    Array<SeratoMarker::OfSomeSort> seratoMarkers() const override
    {
        return OGGTrackFileInternal::markersInComment(*this->xiphComment);
    }
    void setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers) override
    {
        auto decodedData = MutableBlob::withBase64String(OGGTrackFileInternal::seratoMarkersV2Base64StringInComment(*this->xiphComment));
        this->xiphComment->removeFields(oggSeratoMarkersItemName);
        TagLib::String propertyString;
        this->xiphComment->addField(oggSeratoMarkersItemName, propertyString);
        this->replaceGridMarkersFieldInCommentWith(seratoMarkers);
        decodedData = TrackFileInternal::seratoMarkersV2DecodedDataFromAndOldMarkersV2DecodedData(seratoMarkers, decodedData);
        OGGTrackFileInternal::replaceFieldInCommentWith(oggSeratoMarkersV2ItemName, *this->xiphComment, decodedData.base64String());
    }
};

}
