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

#include <TrackFilesCollection/Tracks/Track.hpp>

#include <CommonCollection/Collection.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace TrackFile {

// -- Public Interface
class MutableCollection : public Common::MutableCollection, public Common::Collection
{
    // -- Private Instance Variables
    FilePath p_volumePath;

public:
    // -- Constructors & Destructors
    MutableCollection(FilePath&& volume) : p_volumePath(std::move(volume))
    {
        NXA_ASSERT_TRUE(this->p_volumePath.isADirectory());
    }
    MutableCollection(const Collection& other) : p_volumePath(other.p_volumePath) { }
    ~MutableCollection() = default;

    // -- Common::Collection Overridden Instance Methods
    Optional<Collection::Error> lastOpenResult() const override
    {
        return nothing;
    }
    String name() const override
    {
        return "TrackFile"_String;
    }
    Common::Collection::Type type() const override
    {
        return Common::Collection::Type::TrackFile;
    }
    FilePath volume() const override
    {
        return this->p_volumePath();
    }
    Time lastModificationTime() const override
    {
        NXA_ALOG("Not supported.");
    }

    boolean isOpened() const override
    {
        return true;
    }
    Optional<String> maybeOpeningErrorDescription() const override
    {
        return nothing;
    }
    boolean hasChangesToSave() const override
    {
        return false;
    }

    NotNull<const Common::Folder*> rootFolder() const override
    {
        NXA_ALOG("Not supported.");
    }
    NotNull<const Common::Playlist*> tracks() const override
    {
        NXA_ALOG("Not supported.");
    }

    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithRelativeFilePath(const FilePath& relativeFilePath) const override
    {
        return Track::maybeTrackForPath(FilePath::filePathByJoiningPaths(this->p_volumePath, relativeFilePath));
    }

    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath) const override
    {
        return Common::Collection::p_maybeExistingTrackWithAbsoluteFilePathIn(absoluteFilePath, *this);
    }
};

} }
