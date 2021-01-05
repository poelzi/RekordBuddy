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

#include <TrackFiles/AIFFTrackFile.hpp>

#include "Internal/ID3TrackFileInternal.hpp"

#include <Base/Base.hpp>

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wdelete-non-virtual-dtor"
#pragma clang diagnostic ignored "-Wdocumentation"
#pragma clang diagnostic ignored "-Wshadow"
#endif

#include <tag/aifffile.h>
#include <tag/aiffproperties.h>

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

#include <memory>

namespace NxA {

#define NXA_OBJECT_CLASS                                    AIFFTrackFile
#define NXA_OBJECT_INTERNAL_BASE_CLASS                      TrackFileInternal

// -- Internal Interface
class AIFFTrackFileInternal : public ID3TrackFileInternal
{
    #include <Base/ObjectInternal.ipp>

public:
    // -- Instance Variables
    mutable std::unique_ptr<TagLib::RIFF::AIFF::File> aiffFile;

    // -- Constructors & Destructors
    AIFFTrackFileInternal(const FilePath& path) : ID3TrackFileInternal(path) { }
    ~AIFFTrackFileInternal() override = default;

    // -- Operators
    inline bool operator==(const AIFFTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator==(other);
    }
    inline bool operator<(const AIFFTrackFileInternal& other) const noexcept
    {
        return this->TrackFileInternal::operator<(other);
    }

    // -- Overridden TrackFileInternal Instance methods
    boolean parse() const override
    {
        this->load();

        this->aiffFile = std::make_unique<TagLib::RIFF::AIFF::File>(this->fileStream.get(),
                                                                    true, TagLib::AudioProperties::ReadStyle::Fast);
        if (!this->aiffFile->isValid()) {
            this->aiffFile = nullptr;
            return false;
        }

        this->file = this->aiffFile.get();

        this->id3v2Tag = this->aiffFile->tag();
        if (!this->id3v2Tag) {
            this->aiffFile = nullptr;
            return false;
        }

        return this->ID3TrackFileInternal::parse();
    }
    boolean save() const override
    {
        if (!this->aiffFile.get()) {
            return true;
        }

        return ID3TrackFileInternal::save();
    }

    AudioFileType type() const override
    {
        return AudioFileType::AIFF;
    }

    boolean hasBitDepth() const override
    {
        return true;
    }
    count bitDepthInBits() const override
    {
        return static_cast<TagLib::RIFF::AIFF::Properties*>(this->audioProperties)->bitsPerSample();
    }
};

}
