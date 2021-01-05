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

#include <TrackFiles/AudioFileType.hpp>

#include <TrackFiles/TrackFileFactory.hpp>

namespace NxA {

// -- Local Variables

static MutableMap<FilePath, AudioFileType> p_fileTypeCache;

// -- Functions

AudioFileType exactTypeForAudioFileAt(const FilePath& absolutePath)
{
    auto genericType = genericTypeForAudioFileAt(absolutePath);
    if (genericType != AudioFileType::MP4) {
        return genericType;
    }

    auto maybeCachedValue = p_fileTypeCache.maybeValueForKey(absolutePath);
    if (maybeCachedValue.isValid()) {
        return *maybeCachedValue;
    }

    auto maybeTrackFile = TrackFileFactory::maybeTrackFileForPath(absolutePath);
    if (!maybeTrackFile.isValid()) {
        return AudioFileType::Unknown;
    }

    auto type = maybeTrackFile->type();
    p_fileTypeCache.setValueForKey(type, absolutePath);

    return type;
}

AudioFileType genericTypeForAudioFileAt(const FilePath& absolutePath)
{
    auto maybeFileExtension = absolutePath.maybeFileExtension();
    if (maybeFileExtension.isValid()) {
        auto extension = maybeFileExtension->asEncodedString().lowerCaseString();

        if ((extension == "aif") || (extension == "aiff")) {
            return AudioFileType::AIFF;
        }
        else if (extension == "mp3") {
            return AudioFileType::MP3;
        }
        else if (extension == "m4v") {
            return AudioFileType::Movie;
        }
        else if ((extension == "mp4") || (extension == "m4a")) {
            return AudioFileType::MP4;
        }
        else if (extension == "wav") {
            return AudioFileType::WAV;
        }
        else if (extension == "flac") {
            return AudioFileType::FLAC;
        }
        else if (extension == "ogg") {
            return AudioFileType::OGG;
        }
    }

    return AudioFileType::Unknown;
}

}
