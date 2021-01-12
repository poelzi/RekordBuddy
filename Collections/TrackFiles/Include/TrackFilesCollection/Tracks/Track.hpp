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

#include <TrackFilesCollection/Collection.hpp>

#include <TrackFiles/TrackFile.hpp>

namespace NxA { namespace TrackFile {

// -- Public Interface
class MutableTrack : public Common::MutableTrack, public Common::Track
{
    // -- Private Class Methods
    inline Optional<String> p_stringIfNotEmpty(const String& from)
    {
        return from.length() ? Optional<String>{ from } : Optional<String>{ };
    }

    // -- Private Instance Variables
    MutableCollection p_collection;
    TrackFile p_trackFile;

    // -- Private Constructors & Destructors
    MutableTrack(const FilePath& volumePath, TrackFile&& trackFile) : p_collection{ volumePath },
                                                                      p_trackFile{ std::move(trackFile) } { }

public:
    // -- Factory Methods
    static Optional<NotNull<Common::MutableTrack*>> maybeTrackForPath(const FilePath& path)
    {
        auto maybeVolumePath = path.maybeVolumeIfAny();
        if (!maybeVolumePath.isValid()) {
            return nothing;
        }

        auto maybeTrackFile = TrackFile::maybeTrackForPath(path);
        if (!maybeTrackFile.isValid()) {
            return nothing;
        }

        return NotNull<MutableTrack*>{ *maybeVolumePath, std::move(*maybeTrackFile) };
    }

    // -- Constructors & Destructors
    ~MutableTrack() override = default;

    // -- Instance Methods
    NotNull<const Common::Collection*> collection() const override
    {
        return this->p_collection.asNotNull();
    }
    NotNull<Common::MutableCollection*> collection() override
    {
        return this->p_collection.asNotNull();
    }

    FilePath volume() const override
    {
        return this->p_collection.volume();
    }
    FilePath relativeFilePath() const override
    {
        return *this->p_trackFile.path().maybeWithPrefixRemoved(this->p_collection.volume());
    }
    FilePath absoluteFilePath() const override
    {
        return Common::Track::absolutePathFor(*this);
    }

    Time lastModificationTime() const override
    {
        return Time{ File::modificationTimeForFile(this->p_trackFile.path()) };
    }
    void markAsModifiedNow() override
    {
        return File::setModificationTimeForFile(Time::currentTime(), this->p_trackFile.path());
    }

    Optional<String> maybeTitle() const override
    {
        MutableTrack::p_stringIfNotEmpty(this->p_trackFile.title());
    }
    void setTitle(const Optional<String>& maybeValue) override
    {
        this->p_trackFile.setTitle(maybeValue.valueOr(String{ }));
    }

    Optional<String> maybeAlbum() const override
    {
        MutableTrack::p_stringIfNotEmpty(this->p_trackFile.album());
    }
    void setAlbum(const Optional<String>& maybeValue) override
    {
        this->p_trackFile.setAlbum(maybeValue.valueOr(String{ }));
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
        MutableTrack::p_stringIfNotEmpty(this->p_trackFile.comments());
    }
    void setComments(const Optional<String>& maybeValue) override
    {
        this->p_trackFile.setComments(maybeValue.valueOr(String{ }));
    }

    Array<String> genres() const override
    {
        return this->p_trackFile.genre().splitBySeparator(", ");
    }
    void setGenres(const Array<String>& values) override
    {
        this->p_trackFile.setGenre(String::stringByJoiningArrayWithString(values, ", "));
    }

    Optional<String> maybeGrouping() const override
    {
        MutableTrack::p_stringIfNotEmpty(this->p_trackFile.grouping());
    }
    void setGrouping(const Optional<String>& maybeValue) override
    {
        this->p_trackFile.setGrouping(maybeValue.valueOr(String{ }));
    }

    Optional<String> maybeMixName() const override
    {
        MutableTrack::p_stringIfNotEmpty(this->p_trackFile.mixName());
    }
    void setMixName(const Optional<String>& maybeValue) override
    {
    }

    Optional<String> maybeRecordLabel() const override
    {
        if (this->p_trackFile->hasRecordLabel()) {
            return nothing;
        }

        MutableTrack::p_stringIfNotEmpty(this->p_trackFile.recordLabel());
    }
    void setRecordLabel(const Optional<String>& maybeValue) override
    {
        this->p_trackFile.setRecordLabel(maybeValue.valueOr(String{ }));
    }

    Array<String> tags() const override
    {
        return this->p_trackFile.tags().splitBySeparator(", ");
    }
    void setTags(const Array<String>& values) override
    {
    }

    Array<String> artists() const override
    {
        return this->p_trackFile.artist().splitBySeparator(", ");
    }
    void setArtists(const Array<String>& values) override
    {
        this->p_trackFile.setArtist(String::stringByJoiningArrayWithString(values, ", "));
    }

    Array<String> producers() const override
    {
        return this->p_trackFile.composer().splitBySeparator(", ");
    }
    void setProducers(const Array<String>& values) override
    {
        this->p_trackFile.setComposer(String::stringByJoiningArrayWithString(values, ", "));
    }

    Array<String> remixers() const override
    {
        if (this->p_trackFile->hasRemixer()) {
            return nothing;
        }

        return this->p_trackFile.remixer().splitBySeparator(", ");
    }
    void setRemixers(const Array<String>& values) override
    {
        this->p_trackFile.setRemixer(String::stringByJoiningArrayWithString(values, ", "));
    }

    Optional<Date> maybeDateAdded() const override
    {
        return nothing;
    }
    void setDateAdded(const Optional<Date>& maybeValue) override
    {
        timestamp dateAsTimeStamp = 0;

        if (maybeValue.isValid()) {
            dateAsTimeStamp = maybeValue->asTimeConvertedFromLocalTimeZone().asUnixTimeStamp();
        }

        this->p_trackFile.setDateAddedInSecondsSinceJanuary1st1970(dateAsTimeStamp);
    }

    Optional<boolean> maybeBeatGridLocked() const override
    {
        return nothing;
    }
    void setBeatGridLocked(boolean value) override
    {
        this->p_trackFile.setBeatGridIsLocked(value);
    }

    Optional<count> maybeBitDepthInBits() const override
    {
        if (this->p_trackFile->hasBitDepth()) {
            return nothing;
        }

        return this->p_trackFile->bitDepthInBits();
    }
    void setBitDepthInBits(const Optional<count>& maybeValue) override
    {
    }

    Optional<count> maybeBitRateInKiloBitsPerSecond() const override
    {
        auto result = this->p_trackFile.bitRateInKiloBitsPerSecond();
        return result ? Optional<count>{ result } : nothing;
    }
    void setBitRateInKiloBitsPerSecond(const Optional<count>& maybeValue) override
    {
        this->p_trackFile.setBitRateInKiloBitsPerSecond(maybeValue.valueOr(0));
    }

    Optional<DecimalNumber> maybeBeatsPerMinute() const override
    {
        return this->p_trackFile.bpm().decimalValue();
    }
    void setBeatsPerMinute(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_trackFile.setBpm(maybeValue.isValid() ? maybeValue->asStringWithFractionDigitsBetween(0, 1) : String{ });
    }

    Optional<uinteger32> maybeRgbColor() const override
    {
        auto result = this->p_trackFile.rgbColor();
        return result ? Optional<uinteger32>{ result } : nothing;
    }
    void setRgbColor(const Optional<uinteger32>& maybeValue) override
    {
        this->p_trackFile.setRgbColor(maybeValue.valueOr(0));
    }

    Optional<count> maybeFileSizeInBytes() const override
    {
        auto result = this->p_trackFile.sizeInBytes();
        return result ? Optional<count>{ result } : nothing;
    }
    void setFileSizeInBytes(const Optional<count>& maybeValue) override
    {
        this->p_trackFile.setSizeInBytes(maybeValue.valueOr(0));
    }

    Array<String> musicalKeys() const override
    {
        if (this->p_trackFile->hasKeys()) {
            return { this->p_trackFile.key() };
        }

        return { };
    }
    void setMusicalKeys(const Array<String>& values) override
    {
        if (values.length() > 0) {
            auto maybeConvertedValue = Common::MusicalKey::maybeStringValueInDefaultNotationFromString(values.firstObject());
            if (maybeConvertedValue.isValid()) {
                this->p_trackFile.setKey(*maybeConvertedValue);
                return;
            }
        }

        this->p_trackFile.setKey({ });
    }

    Optional<DecimalNumber> maybeLengthInSeconds() const override
    {
        auto result = this->p_trackFile.lengthInMilliSeconds();
        return result ? Optional<DecimalNumber>{ DecimalNumber::withIntegerAndExponant(*result, -3) : nothing;
    }
    void setLengthInSeconds(const Optional<DecimalNumber>& maybeValue) override
    {
        this->p_trackFile.setLengthInMilliSeconds(maybeValue.isValid() ? (*maybeValue * 1000).asInteger() : 0);
    }

    Optional<count> maybeTrackNumber() const override
    {
        auto result = this->p_trackFile.trackNumber();
        return result ? Optional<count>{ result } : nothing;
    }
    void setTrackNumber(const Optional<count>& maybeValue) override
    {
        this->p_trackFile.setTrackNumber(maybeValue.valueOr(0));
    }

    Optional<count> maybeDiscNumber() const override
    {
        auto result = this->p_trackFile.discNumber();
        return result ? Optional<count>{ result } : nothing;
    }
    void setDiscNumber(const Optional<count>& maybeValue) override
    {
        this->p_trackFile.setDiscNumber(maybeValue.valueOr(0));
    }

    Optional<count> maybeSampleRateInHertz() const override
    {
        auto result = this->p_trackFile.sampleRateInSamplesPerSecond();
        return result ? Optional<count>{ result } : nothing;
    }
    void setSampleRateInHertz(const Optional<count>& maybeValue) override
    {
        this->p_trackFile.setSampleRateInSamplesPerSecond(maybeValue.valueOr(0));
    }
};

} }
