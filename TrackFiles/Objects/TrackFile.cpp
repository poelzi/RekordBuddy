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

#include <TrackFiles/TrackFile.hpp>
#include <TrackFiles/MP4TrackFile.hpp>

#include "Internal/TrackFileInternal.hpp"

using namespace NxA;

#define NXA_OBJECT_CLASS                            TrackFile

#include <Base/ObjectDefinition.ipp>

// -- Class Variables

FilePath TrackFile::p_mp4Extension{ "mp4" };

// -- Class Methods

Optional<String> TrackFile::maybeStringValueFor(AudioFileType type)
{
    switch (type) {
        case AudioFileType::Unknown: {
            return nothing;
        }
        case AudioFileType::MP3: {
            return "MP3"_String;
        }
        case AudioFileType::AIFF: {
            return "AIFF"_String;
        }
        case AudioFileType::WAV: {
            return "WAV"_String;
        }
        case AudioFileType::AAC: {
            return "AAC"_String;
        }
        case AudioFileType::OGG: {
            return "OGG"_String;
        }
        case AudioFileType::FLAC: {
            return "FLAC"_String;
        }
        case AudioFileType::ALAC: {
            return "Apple Lossless"_String;
        }
        case AudioFileType::AACSTEM: {
            return "Stem"_String;
        }
        case AudioFileType::ALACSTEM: {
            return "Stem"_String;
        }
        case AudioFileType::WMA: {
            return "WMA"_String;
        }
        case AudioFileType::Movie: {
            return "Movie"_String;
        }
        case AudioFileType::MP4: {
            return "MP4"_String;
        }
    }

    return nothing;
}

// -- Instance Methods

FilePath TrackFile::path() const
{
    return nxa_const_internal->filePath;
}

count TrackFile::sizeInBytes() const
{
    return nxa_const_internal->sizeInBytes();
}

AudioFileType TrackFile::type() const
{
    return nxa_const_internal->type();
}

DecimalNumber TrackFile::offsetToAddToMarkerPositionsForRekordboxInSeconds() const
{
    return nxa_const_internal->offsetToAddToMarkerPositionsForRekordboxInSeconds();
}

String TrackFile::title() const
{
    return nxa_const_internal->title();
}

void TrackFile::setTitle(String title)
{
    if (title.hasNonPrintableCharacters()) {
        title = String::stringByFilteringNonPrintableCharactersIn(title);
    }

    nxa_internal->setTitle(title);
}

String TrackFile::artist() const
{
    return nxa_const_internal->artist();
}

void TrackFile::setArtist(String artist)
{
    if (artist.hasNonPrintableCharacters()) {
        artist = String::stringByFilteringNonPrintableCharactersIn(artist);
    }

    nxa_internal->setArtist(artist);
}

String TrackFile::genre() const
{
    return nxa_const_internal->genre();
}

void TrackFile::setGenre(String genre)
{
    if (genre.hasNonPrintableCharacters()) {
        genre = String::stringByFilteringNonPrintableCharactersIn(genre);
    }

    nxa_internal->setGenre(genre);
}

String TrackFile::comments() const
{
    return nxa_const_internal->comments();
}

void TrackFile::setComments(String comments)
{
    if (comments.hasNonPrintableCharacters()) {
        comments = String::stringByFilteringNonPrintableCharactersIn(comments);
    }

    nxa_internal->setComments(comments);
}

String TrackFile::album() const
{
    return nxa_const_internal->album();
}

void TrackFile::setAlbum(String album)
{
    if (album.hasNonPrintableCharacters()) {
        album = String::stringByFilteringNonPrintableCharactersIn(album);
    }

    nxa_internal->setAlbum(album);
}

count TrackFile::trackNumber() const
{
    return nxa_const_internal->trackNumber();
}

void TrackFile::setTrackNumber(count trackNumber)
{
    nxa_internal->setTrackNumber(trackNumber);
}

String TrackFile::releaseDate() const
{
    return nxa_const_internal->releaseDate();
}

void TrackFile::setReleaseDate(String date)
{
    if (date.hasNonPrintableCharacters()) {
        date = String::stringByFilteringNonPrintableCharactersIn(date);
    }

    nxa_internal->setReleaseDate(date);
}

NxA::boolean TrackFile::hasKey() const
{
    return nxa_const_internal->hasKey();
}

String TrackFile::key() const
{
    return nxa_internal->key();
}

void TrackFile::setKey(String key)
{
    if (key.hasNonPrintableCharacters()) {
        key = String::stringByFilteringNonPrintableCharactersIn(key);
    }

    nxa_internal->setKey(key);
}

String TrackFile::composer() const
{
    return nxa_const_internal->composer();
}

void TrackFile::setComposer(String composer)
{
    if (composer.hasNonPrintableCharacters()) {
        composer = String::stringByFilteringNonPrintableCharactersIn(composer);
    }

    nxa_internal->setComposer(composer);
}

String TrackFile::grouping() const
{
    return nxa_const_internal->grouping();
}

void TrackFile::setGrouping(String grouping)
{
    if (grouping.hasNonPrintableCharacters()) {
        grouping = String::stringByFilteringNonPrintableCharactersIn(grouping);
    }

    nxa_internal->setGrouping(grouping);
}

String TrackFile::bpm() const
{
    return nxa_const_internal->bpm();
}

void TrackFile::setBpm(String bpm)
{
    if (bpm.hasNonPrintableCharacters()) {
        bpm = String::stringByFilteringNonPrintableCharactersIn(bpm);
    }

    nxa_internal->setBpm(bpm);
}

NxA::boolean TrackFile::hasRecordLabel() const
{
    return nxa_const_internal->hasRecordLabel();
}

String TrackFile::recordLabel() const
{
    return nxa_const_internal->recordLabel();
}

void TrackFile::setRecordLabel(String recordLabel)
{
    if (recordLabel.hasNonPrintableCharacters()) {
        recordLabel = String::stringByFilteringNonPrintableCharactersIn(recordLabel);
    }

    nxa_internal->setRecordLabel(recordLabel);
}

NxA::boolean TrackFile::hasRemixer() const
{
    return nxa_const_internal->hasRemixer();
}

String TrackFile::remixer() const
{
    return nxa_const_internal->remixer();
}

void TrackFile::setRemixer(String remixer)
{
    if (remixer.hasNonPrintableCharacters()) {
        remixer = String::stringByFilteringNonPrintableCharactersIn(remixer);
    }

    nxa_internal->setRemixer(remixer);
}

Optional<uinteger> TrackFile::maybeRating() const
{
    return nxa_const_internal->maybeRating();
}

void TrackFile::setRating(const Optional<uinteger>& rating)
{
    nxa_internal->setRating(rating);
}

String TrackFile::tags(void) const
{
    return nxa_const_internal->tags();
}

void TrackFile::setTags(String tags)
{
    nxa_internal->setTags(tags);
}

Blob TrackFile::artwork() const
{
    return nxa_const_internal->artwork();
}

void TrackFile::setArtwork(const Blob& artwork)
{
    nxa_internal->setArtwork(artwork);
}

count TrackFile::audioDataSizeInBytes() const
{
    return nxa_const_internal->audioDataSizeInBytes();
}

count TrackFile::lengthInMilliseconds() const
{
    return nxa_const_internal->lengthInMilliseconds();
}

count TrackFile::bitRateInKiloBitsPerSecond() const
{
    return nxa_const_internal->bitRateInKiloBitsPerSecond();
}

NxA::boolean TrackFile::hasBitDepth() const
{
    return nxa_const_internal->hasBitDepth();
}

count TrackFile::bitDepthInBits() const
{
    return nxa_const_internal->bitDepthInBits();
}

count TrackFile::sampleRateInSamplesPerSecond() const
{
    return nxa_const_internal->sampleRateInSamplesPerSecond();
}

NxA::boolean TrackFile::seratoBeatGridIsLocked() const
{
    return nxa_const_internal->seratoBeatGridIsLocked();
}

void TrackFile::setSeratoBeatGridIsLocked(boolean locked)
{
    nxa_internal->setSeratoBeatGridIsLocked(locked);
}

Array<SeratoMarker::OfSomeSort> TrackFile::seratoMarkers() const
{
    return nxa_const_internal->seratoMarkers();
}

void TrackFile::setSeratoMarkers(const MutableArray<SeratoMarker::OfSomeSort>& seratoMarkers)
{
    nxa_internal->setSeratoMarkers(seratoMarkers);
}

bool TrackFile::save() const
{
    return nxa_const_internal->save();
}
