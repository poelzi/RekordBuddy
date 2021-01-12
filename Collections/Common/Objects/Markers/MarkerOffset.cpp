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

#include <CommonCollection/Markers/MarkerOffset.hpp>

#include <TrackFiles/TrackFile.hpp>

#include <stdio.h>

// -- Static Methods

namespace NxA { namespace Common {

static const int p_mp3Frequencies[] = { 44100, 48000, 32000, 22050, 24000, 16000, 11025, 12000, 8000 };

static int p_readSyncInteger(NotNull<const unsigned char*> buffer)
{
    return (buffer[0] << 21) | (buffer[1] << 14) | (buffer[2] << 7) | buffer[3];
}

static int p_readInteger(NotNull<const unsigned char*> buffer)
{
    return (buffer[0] << 24) | (buffer[1] << 16) | (buffer[2] << 8) | buffer[3];
}

static int p_readID3(NotNull<const unsigned char*> buffer)
{
    if ((buffer[0] != 'I') || (buffer[1] != 'D') || (buffer[2] != '3')) {
        return 0;
    }

#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    int id3_version = buffer[3];
    NXA_DLOG_WITH_FORMAT("   ID3 version 2.%d\n", id3_version);
#endif

    int tag_size = p_readSyncInteger(buffer + 6);
    return tag_size;
}

static integer64 p_positionOffsetInMilliSecondsToToAddWhenImportingRekordboxMarkersForMP3FileAtPath(const FilePath& path)
{
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("file at '%s'\n", path.asEncodedString().asUTF8());
#endif

    if (!File::existsAt(path)) {
        return 0;
    }

    File f { path };
    if (!f.open(combineAccessModes(File::AccessMode::Read, File::AccessMode::Binary))) {
        return 0;
    }
    auto filesize = f.seek(0, File::SeekMode::FromEnd);

#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("   file size: %ld (0x%lx)\n", filesize, filesize);
#endif

    f.seek(0, File::SeekMode::FromStart);

    if (filesize < 10) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: File is too small\n");
#endif
        return 0;
    }

    auto prefix = f.read(10);

    if (prefix.isEmpty() || prefix.size() != 10) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: failed to read prefix\n");
#endif
        return 0;
    }

    auto tag_size = p_readID3(prefix.data());
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("   tag size: %d (0x%x)\n", tag_size, tag_size);
#endif
    if (tag_size < 0 || tag_size > filesize) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: failed to read ID3 data\n");
#endif
        return 0;
    }

    f.seek(tag_size, File::SeekMode::FromCurrent);

    // -- From https://www.mp3-tech.org/programmer/frame_header.html
    auto frameHeader = f.read(44);
    if (frameHeader.isEmpty() || frameHeader.size() != 44) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: failed to read frame header\n");
#endif
        return 0;
    }

    auto mpa_version = (frameHeader[1] >> 3) & 3;
    auto sample_rate_index = (frameHeader[2] >> 2) & 3;
    if (sample_rate_index == 3) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: failed to read ID3 data\n");
#endif
        return 0;
    }

    int sample_rate;
    switch (mpa_version) {
        case 0: // MPEG Audio v2.5
            sample_rate = p_mp3Frequencies[sample_rate_index + 6];
            break;

        case 1: // reserved
            sample_rate = -1;
            break;

        case 2: // MPEG Audio v2
            sample_rate = p_mp3Frequencies[sample_rate_index + 3];
            break;

        case 3: // MPEG Audio v1
            sample_rate = p_mp3Frequencies[sample_rate_index];
            break;
    }

#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("   sample rate appears to be %d Hz\n", sample_rate);
#endif

    // --  From http://gabriel.mp3-tech.org/mp3infotag.html
    if ((strncmp(reinterpret_cast<const char*>(frameHeader.data().get()) + 36, "Info", 4) != 0) &&
        (strncmp(reinterpret_cast<const char*>(frameHeader.data().get()) + 36, "Xing", 4) != 0)) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: failed to read info data\n");
#endif
        return 0;
    }

#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("   found tag named '%.4s' ", frameHeader.data()+36);
#endif

    auto header_flags = p_readInteger(frameHeader.data() + 40);
    auto is_vbr = (header_flags & 0x8) ? 1 : 0;
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("   (%s)\n", is_vbr?"VBR":"CBR");
#endif

    auto skip = ((header_flags & 1) ? 4 : 0) + ((header_flags & 2) ? 4 : 0) + ((header_flags & 4) ? 100 : 0);
    f.seek(skip, File::SeekMode::FromCurrent);

    int vbr_quality = -1;
    if (is_vbr) {
        auto vbrInt = f.read(4);
        if (vbrInt.isEmpty() || vbrInt.size() != 4) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
            printf("   Error: failed to read vbr data\n");
#endif
            return 0;
        }
        vbr_quality = p_readInteger(vbrInt.data());
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   VBR quality: %d\n", vbr_quality);
#endif
    }

    auto encoderData = f.read(24);
    if (encoderData.isEmpty() || encoderData.size() != 24) {
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
        printf("   Error: failed to read lame data\n");
#endif
        return 0;
    }

    auto is_lame = strncmp(reinterpret_cast<const char*>(encoderData.data().get()), "LAME", 4) == 0;
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    printf("   Encoder: '%.*s'\n", is_lame ? 9 : 20, encoderData.data());
#endif

    auto encoder_delay = (encoderData[21] << 4) | (encoderData[22] >> 4);
#if defined(NXA_DEBUG_MP3_OFFSET_CODE)
    auto tail_padding = (encoderData[22] << 8) | encoderData[23];
        printf("   pad info: %d %d\n", encoder_delay, tail_padding);
#endif

    return is_lame ? 0 : (2 * encoder_delay * 1000.0f) / sample_rate;
}

} }

using namespace NxA;
using namespace NxA::Common;

// -- Class Methods

Optional<DecimalNumber> MarkerOffset::maybeOffsetToAddInSecondsForFileAtWhenImportingFrom(const FilePath& filePath, Common::Collection::Type collectionType)
{
    switch (collectionType) {
        case Collection::Type::rekordbox: {
            auto fileType = genericTypeForAudioFileAt(filePath);
            if (fileType == AudioFileType::MP3) {
                auto offset = p_positionOffsetInMilliSecondsToToAddWhenImportingRekordboxMarkersForMP3FileAtPath(filePath);
                if (!offset) {
                    return nothing;
                }

                return DecimalNumber::withIntegerAndExponant(offset, -3);
            }
            else if (fileType == AudioFileType::MP4) {
                // -- rekordbox and the CDJs have a bug in their ACC M4A code that causes about 50ms of silence to be inserted a the beginning of a track
                // -- because of this we need to offset the markers accordingly so they still match other programs
                return DecimalNumber::withIntegerAndExponant(-48, -3);
            }

            return nothing;
        }
        default: {
            // -- For now all other collections just return no offset
            return nothing;
        }
    }
}
