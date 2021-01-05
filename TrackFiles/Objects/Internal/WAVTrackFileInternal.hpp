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

#include <TrackFiles/WAVTrackFile.hpp>

#include "Internal/ID3TrackFileInternal.hpp"

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#include <tag/wavfile.h>
#include <tag/wavproperties.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

namespace NxA {

#define NXA_OBJECT_CLASS                WAVTrackFile
#define NXA_OBJECT_INTERNAL_BASE_CLASS  TrackFileInternal

// -- Internal Interface
class WAVTrackFileInternal : public ID3TrackFileInternal
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Instance Variables
    mutable std::unique_ptr<TagLib::RIFF::WAV::File> wavFile;

    // -- Constructor & Destructors
    WAVTrackFileInternal(const FilePath& path) : ID3TrackFileInternal(path) { }
    ~WAVTrackFileInternal() override = default;

    // -- Operators
    inline bool operator==(const WAVTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator==(other);
    }
    inline bool operator<(const WAVTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator<(other);
    }

    // -- Overridden TrackFileInternal Instance Methods
    boolean parse() const override
    {
        this->load();

        this->wavFile = std::make_unique<TagLib::RIFF::WAV::File>(this->fileStream.get(),
                                                                  true, TagLib::AudioProperties::ReadStyle::Fast);
        if (!this->wavFile->isValid()) {
            this->wavFile = nullptr;
            return false;
        }

        this->file = this->wavFile.get();

        this->id3v2Tag = this->wavFile->ID3v2Tag();
        if (!this->id3v2Tag) {
            this->wavFile = nullptr;
            return false;
        }

        return this->ID3TrackFileInternal::parse();
    }
    boolean save() const override
    {
        if (!this->wavFile.get()) {
            return true;
        }

        return ID3TrackFileInternal::save();
    }

    AudioFileType type() const override
    {
        return AudioFileType::WAV;
    }

    boolean hasBitDepth() const override
    {
        return true;
    }
    count bitDepthInBits() const override
    {
        return static_cast<TagLib::RIFF::WAV::Properties*>(this->audioProperties)->bitsPerSample();
    }
};

}
