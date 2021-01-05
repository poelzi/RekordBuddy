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

#include <TrackFiles/MPEGTrackFile.hpp>

#include "Internal/ID3TrackFileInternal.hpp"

#include <Base/Base.hpp>

#include <tag/mpegfile.h>
#include <tag/mpegproperties.h>

namespace NxA {

// -- Private Functions

static const int p_mp3_frequencies[] = { 44100, 48000, 32000, 22050, 24000, 16000, 11025, 12000, 8000 };

static int p_read_syncint(const char *buffer) {
    int a = buffer[0];
    int b = buffer[1];
    int c = buffer[2];
    int d = buffer[3];

    return (a << 21) | (b << 14) | (c << 7) | d;
}

static int p_read_int(const char *buffer) {
    int a = buffer[0];
    int b = buffer[1];
    int c = buffer[2];
    int d = buffer[3];

    return (a << 24) | (b << 16) | (c << 8) | d;
}

static int p_read_id3(const char *buffer) {
    int id3_version;
    int tag_size;
    if ((buffer[0] != 'I') || (buffer[1] != 'D') || (buffer[2] != '3')) return 0;
    id3_version = buffer[3];
#if DEBUG_ME
    printf("   ID3 version 2.%d\n", id3_version);
#endif
    tag_size = p_read_syncint(buffer + 6);
    return tag_size;
}

#define NXA_OBJECT_CLASS                                    MPEGTrackFile
#define NXA_OBJECT_INTERNAL_BASE_CLASS                      TrackFileInternal

// -- Internal Interface
class MPEGTrackFileInternal : public ID3TrackFileInternal
{
#include <Base/ObjectInternal.ipp>

public:
    // -- Instance Variables
    mutable std::unique_ptr<TagLib::MPEG::File> mpegFile;

    mutable Optional<DecimalNumber> maybeOffsetToAddToMarkerPositionsForRekordboxInSeconds;

    // -- Constructor & Destructors
    MPEGTrackFileInternal(const FilePath &path) : ID3TrackFileInternal(path) { }
    ~MPEGTrackFileInternal() override = default;

    // -- Operators
    bool operator==(const MPEGTrackFileInternal &other) const noexcept
    {
        return this->TrackFileInternal::operator==(other);
    }

    bool operator<(const MPEGTrackFileInternal &other) const noexcept
    {
        return this->TrackFileInternal::operator<(other);
    }

    // -- Instance Methods
    DecimalNumber positionOffsetToAddToRekordboxMarkersInSeconds() const
    {
#if DEBUG_ME
        printf("file at '%s'\n", this->filePath.stringValue().asUTF8());
#endif
        long filesize = this->sizeInBytes();
        if (filesize < 10) {
            return { };
        }

#if DEBUG_ME
        printf("   file size: %ld (0x%lx)\n", filesize, filesize);
#endif

        this->load();

        // -- Some MP3 files encoded by LAME don't seem to have the correct encoder string so
        // -- programs like Serato and Traktor don't read/skip the gapless frames in the audio correctly.
        // -- This means we need to adjust the timing of our markers when converting between programs.
        // -- Since rekorbox seems to be the odd one out (they actually get it right), we fix the markers
        // -- coming from it using an offset we read from the file. See `LAME encoded MP3s gapless bug.md`
        // -- in the Documentation folder of the repo.
        auto& stream = *this->fileStream;

        auto buffer = stream.readBlock(10);
        count bytes_read = buffer.size();
        if (bytes_read != 10) {
            return { };
        }

        int tag_size = p_read_id3(buffer.data());
        if (tag_size < 0) {
            return { };
        }
#if DEBUG_ME
        printf("   tag size: %d (0x%x)\n", tag_size, tag_size);
#endif

        if (tag_size > filesize) {
            return { };
        }
        stream.seek(tag_size, TagLib::IOStream::Position::Current);

        // -- From https://www.mp3-tech.org/programmer/frame_header.html
        buffer = stream.readBlock(44);
        bytes_read = buffer.size();
        if (bytes_read != 44) {
            return { };
        }

        int mpa_version = (buffer[1] >> 3) & 3;
        int sample_rate_index = (buffer[2] >> 2) & 3;
        if (sample_rate_index == 3) {
            return { };
        }

        int sample_rate;
        switch (mpa_version) {
            case 0: {
                // -- MPEG Audio v2.5
                sample_rate = p_mp3_frequencies[sample_rate_index + 6];
                break;
            }
            case 1: {
                // -- reserved
                sample_rate = -1;
                break;
            }
            case 2: {
                // -- MPEG Audio v2
                sample_rate = p_mp3_frequencies[sample_rate_index + 3];
                break;
            }
            case 3: {
                // -- MPEG Audio v1
                sample_rate = p_mp3_frequencies[sample_rate_index];
                break;
            }
        }

#if DEBUG_ME
        printf("   sample rate appears to be %d Hz\n", sample_rate);
#endif

        // --  From http://gabriel.mp3-tech.org/mp3infotag.html
        if ((strncmp(buffer.data() + 36, "Info", 4) != 0) &&
            (strncmp(buffer.data() + 36, "Xing", 4) != 0)) {
            return { };
        }

#if DEBUG_ME
        printf("   found tag named '%.4s' ", buffer.data() + 36);
#endif

        int header_flags = p_read_int(buffer.data() + 40);
        int is_vbr = (header_flags & 0x8) ? 1 : 0;
#if DEBUG_ME
        printf("   (%s)\n", is_vbr?"VBR":"CBR");
#endif

        int skip = ((header_flags & 1) ? 4 : 0) + ((header_flags & 2) ? 4 : 0) + ((header_flags & 4) ? 100 : 0);
        stream.seek(skip, TagLib::IOStream::Position::Current);

        if (is_vbr) {
            buffer = stream.readBlock(4);
            bytes_read = buffer.size();
            if (bytes_read != 4) {
                return { };
            }

#if DEBUG_ME
            int vbr_quality = p_read_int(buffer.data());
            printf("   VBR quality: %d\n", vbr_quality);
#endif
        }

        buffer = stream.readBlock(24);
        bytes_read = buffer.size();
        if (bytes_read != 24) {
            return { };
        }

        buffer.append(0);
        int is_lame = strncmp(buffer.data(), "LAME", 4) == 0;
#if DEBUG_ME
        printf("   Encoder: '%.*s'\n", is_lame ? 9 : 20, buffer.data());
#endif

        if (is_lame) {
            return { };
        }

        int encoder_delay = (buffer[21] << 4) | (buffer[22] >> 4);
#if DEBUG_ME
        int tail_padding = (buffer[22] << 8) | buffer[23];
        printf("   pad info: %d %d\n", encoder_delay, tail_padding);
#endif

        return DecimalNumber::withDouble((-2 * encoder_delay * 1000.0f) / sample_rate);
    }

    // -- Overridden TrackFileInternal Instance Methods
    boolean parse() const override
    {
        this->load();

        this->mpegFile = std::make_unique<TagLib::MPEG::File>(this->fileStream.get(), TagLib::ID3v2::FrameFactory::instance(),
                                                              true, TagLib::AudioProperties::ReadStyle::Fast);
        if (!this->mpegFile->isValid()) {
            this->mpegFile = nullptr;
            return false;
        }

        this->file = this->mpegFile.get();

        this->id3v2Tag = this->mpegFile->ID3v2Tag();
        if (!this->id3v2Tag) {
            this->mpegFile = nullptr;
            return false;
        }

        return this->ID3TrackFileInternal::parse();
    }
    boolean save() const override
    {
        if (!this->mpegFile.get()) {
            return true;
        }

        return ID3TrackFileInternal::save();
    }

    AudioFileType type() const override
    {
        return AudioFileType::MP3;
    }
    DecimalNumber offsetToAddToMarkerPositionsForRekordboxInSeconds() const override
    {
        if (!this->maybeOffsetToAddToMarkerPositionsForRekordboxInSeconds.isValid()) {
            this->load();

            auto offsetInSeconds = this->positionOffsetToAddToRekordboxMarkersInSeconds();
            this->maybeOffsetToAddToMarkerPositionsForRekordboxInSeconds = offsetInSeconds;
        }

        return *this->maybeOffsetToAddToMarkerPositionsForRekordboxInSeconds;
    }
};

}
