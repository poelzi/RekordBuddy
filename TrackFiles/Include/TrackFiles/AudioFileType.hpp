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

#include <Base/Base.hpp>

namespace NxA {

// -- Constants
enum class AudioFileType : uinteger32 {
    // -- These are stored in user data and should not be modified.
    // -- New IDs can be added to the list though as long as the maximum value is updated.
    Unknown,
    MP3,
    AIFF,
    WAV,
    AAC,
    OGG,
    FLAC,
    ALAC,
    AACSTEM,
    ALACSTEM,
    WMA,
    Movie,

    // -- This is a generic type for Movie, AAC, ALAC, AACSTEM and ALACSTEM.
    // -- It can be provided on request instead of forcing type() to actually
    // -- load the file in order to inspect its content.
    MP4,

    Maximum = MP4,
};

// -- Public Interface
AudioFileType exactTypeForAudioFileAt(const FilePath&);

// -- This will try to determine the type of file but will never actually load or open the file itself.
AudioFileType genericTypeForAudioFileAt(const FilePath&);

}
