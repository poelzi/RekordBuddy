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

#include <AppleMusicCollection/Tracks/Track.hpp>
#include <AppleMusicCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::AppleMusic;

// -- Factory Methods

Optional<Shared<Track>> Track::maybeTrackWithNodeInCollection(XMLNode track, Pointer<Collection> inCollection, const Protected&)
{
    auto maybeTrackInfoNode = track.maybeFirstSubNode();
    while (maybeTrackInfoNode.isValid()) {
        if (maybeTrackInfoNode->name() == "key"_String) {
            auto maybeKeyName = maybeTrackInfoNode->maybeValue();
            if (maybeKeyName.isValid()) {
                maybeTrackInfoNode = maybeTrackInfoNode->maybeSiblingNode();
                auto maybeValue = maybeTrackInfoNode->maybeValue();

                if ((maybeKeyName == "Location"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                    String subString = maybeValue->asStringByRemovingPercentEncoding();
#if defined(NXA_PLATFORM_MACOS)
                    if (subString.hasPrefix("file://localhost/Volumes/")) {
                        subString = subString.subString(16);
                    }
#elif defined(NXA_PLATFORM_WINDOWS)
                    if (subString.hasPrefix("file://localhost/")) {
                        subString = subString.subString(17);
                    }
#else
                    #error Unsupported platform.
#endif
                    else if (subString.hasPrefix("file://")) {
                        subString = subString.subString(7);
                    }

                    // -- We have to make sure this track has a valid absolute path on a real volume
                    auto absolutePath = FilePath{ subString };
                    if (!absolutePath.maybeRelativeToVolume().isValid()) {
                        return nothing;
                    }

                    return Shared<Track>::with(track, absolutePath, inCollection, Track::p_isProtected);
                }
            }
        }

        maybeTrackInfoNode = maybeTrackInfoNode->maybeSiblingNode();
    }

    return nothing;
}

// -- Constructors & Destructors

Track::Track(XMLNode track,
             const FilePath& absolutePath,
             Pointer<Collection> inCollection,
             const Protected&) : p_collection{ inCollection },
                                 p_appleMusicTrack{ std::move(track) },
                                 p_absoluteFilePath{ absolutePath } { }

// -- Instance Methods

void Track::p_ensureTrackDataIsLoaded() const
{
    if (this->p_trackInfoLoaded) {
        return;
    }

    auto maybeTrackInfoNode = this->p_appleMusicTrack.maybeFirstSubNode();
    while (maybeTrackInfoNode.isValid()) {
        if (maybeTrackInfoNode->name() == "key"_String) {
            auto maybeKeyName = maybeTrackInfoNode->maybeValue();
            if (maybeKeyName.isValid()) {
                maybeTrackInfoNode = maybeTrackInfoNode->maybeSiblingNode();
                auto maybeValue = maybeTrackInfoNode->maybeValue();

                if (maybeKeyName.isValid() && maybeValue.isValid()) {
                    if ((maybeKeyName == "Size"_String) && (maybeTrackInfoNode->name() == "integer"_String)) {
                        this->p_maybeFileSizeInBytes = maybeValue->integerValue();
                    }
                    else if ((maybeKeyName == "Total Time"_String) &&
                             (maybeTrackInfoNode->name() == "integer"_String)) {
                        this->p_maybeLengthInSeconds = DecimalNumber::withInteger(maybeValue->integerValue());
                    }
                    else if ((maybeKeyName == "Track Number"_String) &&
                             (maybeTrackInfoNode->name() == "integer"_String)) {
                        this->p_maybeTrackNumber = maybeValue->integerValue();
                    }
                    else if ((maybeKeyName == "Year"_String) && (maybeTrackInfoNode->name() == "integer"_String)) {
                        if (!this->p_maybeDateReleased.isValid()) {
                            this->p_maybeDateReleased = Date::maybeDateWithYear(maybeValue->integerValue());
                        }
                    }
                    else if ((maybeKeyName == "BPM"_String) && (maybeTrackInfoNode->name() == "integer"_String)) {
                        this->p_maybeBeatsPerMinute = DecimalNumber::withInteger(maybeValue->integerValue());
                    }
                    else if ((maybeKeyName == "Date Modified"_String) &&
                             (maybeTrackInfoNode->name() == "date"_String)) {
                        auto maybeTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(*maybeValue,
                                                                                             "%Y-%m-%dT%H:%M:%SZ");
                        if (maybeTime.isValid()) {
                            this->p_lastModificationTime = *maybeTime;
                        }
                    }
                    else if ((maybeKeyName == "Date Added"_String) && (maybeTrackInfoNode->name() == "date"_String)) {
                        this->p_maybeDateAdded = Date::maybeDateWithString(*maybeValue);
                    }
                    else if ((maybeKeyName == "Bit Rate"_String) && (maybeTrackInfoNode->name() == "integer"_String)) {
                        this->p_maybeBitRateInKiloBitsPerSecond = maybeValue->integerValue();
                    }
                    else if ((maybeKeyName == "Sample Rate"_String) &&
                             (maybeTrackInfoNode->name() == "integer"_String)) {
                        this->p_maybeSampleRateInHertz = maybeValue->integerValue();
                    }
                    else if ((maybeKeyName == "Release Date"_String) && (maybeTrackInfoNode->name() == "date"_String)) {
                        this->p_maybeDateReleased = Date::maybeDateWithString(*maybeValue);
                    }
                    else if ((maybeKeyName == "Name"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        this->p_maybeTitle = *maybeValue;
                    }
                    else if ((maybeKeyName == "Artist"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        this->p_artists = maybeValue->splitBySeparator(this->p_collection.asReference().artistsSeparator());
                    }
                    else if ((maybeKeyName == "Album"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        this->p_maybeAlbum = *maybeValue;
                    }
                    else if ((maybeKeyName == "Genre"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        this->p_genres = maybeValue->splitBySeparator(this->p_collection.asReference().genresSeparator());
                    }
                    else if ((maybeKeyName == "Kind"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        static Map<String, AudioFileType> fileTypeStringToType = {
                                { "MP3 Audio File"_String,  AudioFileType::MP3 },
                                { "AIFF Audio File"_String, AudioFileType::AIFF },
                                { "WAV Audio File"_String,  AudioFileType::WAV },
                                { "M4A Audio File"_String,  AudioFileType::MP4 },
                                { "OGG Audio File"_String,  AudioFileType::OGG },
                                { "FLAC Audio File"_String, AudioFileType::FLAC },
                        };

                        auto maybeType = fileTypeStringToType.maybeValueForKey(*maybeValue);
                        if (maybeType.isValid()) {
                            this->p_fileType = *maybeType;
                        }
                    }
                    else if ((maybeKeyName == "Comments"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        this->p_maybeComments = *maybeValue;
                    }
                    else if ((maybeKeyName == "Work"_String) && (maybeTrackInfoNode->name() == "string"_String)) {
                        this->p_maybeRecordLabel = *maybeValue;
                    }
                }
            }
        }

        maybeTrackInfoNode = maybeTrackInfoNode->maybeSiblingNode();
    }

    this->p_trackInfoLoaded = true;
}

NotNull<const Common::Collection*> Track::collection() const
{
    return this->p_collection.asNotNull();
}
