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

#include <TrackFiles/TrackFileFactory.hpp>
#include <TrackFiles/AIFFTrackFile.hpp>
#include <TrackFiles/MPEGTrackFile.hpp>
#include <TrackFiles/MP4TrackFile.hpp>
#include <TrackFiles/WAVTrackFile.hpp>
#include <TrackFiles/OGGTrackFile.hpp>
#include <TrackFiles/FLACTrackFile.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

using namespace NxA;

// -- Class Methods

Optional<TrackFile> TrackFileFactory::maybeTrackFileForPath(const FilePath& trackFilePath)
{
    if (Directory{ trackFilePath }.exists() || !File::existsAt(trackFilePath) || !trackFilePath.asEncodedString().length()) {
        // -- If the path is one for a folder or if the file doesn't exists then we ignore it.
        return nothing;
    }

    switch (genericTypeForAudioFileAt(trackFilePath)) {
        case AudioFileType::AIFF: {
            return AIFFTrackFile::maybeFileWithFileAt(trackFilePath);
        }
        case AudioFileType::MP3: {
            return MPEGTrackFile::maybeFileWithFileAt(trackFilePath);
        }
        case AudioFileType::MP4: {
            return MP4TrackFile::maybeFileWithFileAt(trackFilePath);
        }
        case AudioFileType::FLAC: {
            return FLACTrackFile::maybeFileWithFileAt(trackFilePath);
        }
        case AudioFileType::OGG: {
            return OGGTrackFile::maybeFileWithFileAt(trackFilePath);
        }
        case AudioFileType::WAV: {
            return WAVTrackFile::maybeFileWithFileAt(trackFilePath);
        }
        default: {
            return nothing;
        }
    }

    return nothing;
}
