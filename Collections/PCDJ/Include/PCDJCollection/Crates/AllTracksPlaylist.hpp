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

#include <PCDJCollection/Tracks/Track.hpp>

#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/CratePath.hpp>

#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>

namespace NxA { namespace PCDJ {

// -- Forward Declarations
class MutableCollection;

// -- Public Interface
class MutableAllTracksPlaylist : public Common::MutablePlaylist, public Common::Playlist
{
    // -- Friends
    friend class MutableCollection;

    // -- Private Instance Variables
    Pointer<MutableCollection> p_collection;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Methods
    Optional<count> p_addExistingTrackAtIndex(NotNull<Common::MutableTrack*>, count) override;

public:
    // -- Constructors & Destructors
    MutableAllTracksPlaylist(Pointer<MutableCollection> inCollection, const Protected&);
    ~MutableAllTracksPlaylist() override = default;

    // -- Instance Methods
    NotNull<const Common::Collection*> collection() const override;
    NotNull<Common::MutableCollection*> collection() override;

    Optional<NotNull<const Common::Folder*>> maybeParentFolder() const override
    {
        return nothing;
    }
    Optional<NotNull<Common::MutableFolder*>> maybeParentFolder() override
    {
        return nothing;
    }

    Time lastModificationTime() const override;
    String name() const override
    {
        return "<All Tracks Playlist>"_String;
    }
    void setName(const String&) override
    {
        NXA_ALOG("This playlist cannot be renamed.");
    }
    inline Common::CratePath path() const override
    {
        return Common::Folder::cratePathFor(*this);
    }
    inline const character* iconName() const override
    {
        return "Playlist Icon/MusicPlaylist invert";
    }

    inline count numberOfSubCrates() const override
    {
        return 0;
    }
    Common::SubCrate subCrateAtIndex(count) const override
    {
        NXA_ALOG("This playlist doesn't have subcrates.");
    }
    Common::MutableSubCrate subCrateAtIndex(count) override
    {
        NXA_ALOG("This playlist doesn't have subcrates.");
    }

    count numberOfTracks() const override;
    NotNull<const Common::Track*> trackAtIndex(count) const override;
    NotNull<Common::MutableTrack*> trackAtIndex(count) override;
    Array<NotNull<const Common::Track*>> tracks() const override
    {
        return Common::Playlist::tracksIn<NotNull<const Common::Track*>>(*this);
    }
    Array<NotNull<Common::MutableTrack*>> tracks() override
    {
        return Common::Playlist::tracksIn<NotNull<Common::MutableTrack*>>(*this);
    }
    inline boolean canReceive(NotNull<const Common::Track*>) const override
    {
        return true;
    }
    inline boolean canReceive(NotNull<Common::MutableTrack*>) const override
    {
        return true;
    }
    void moveTracksAtIndicesToIndex(Array<count>, count) override;
    void moveTrackAtIndexTo(count, count) override;
    void removeTrackAtIndex(count) override;
    void removeAllTracks() override;

    inline boolean isOrganizedBy(const Common::Tag&) const override
    {
        return false;
    }
    void organizeBy(Common::MutableTag&) override
    {
        NXA_ALOG("This playlist can't be organized.");
    }
    void removeOrganizationBy(Common::MutableTag&) override
    {
        NXA_ALOG("This playlist can't be organized.");
    }

    inline boolean isOrganizedBy(Common::Property::TypeID) const override
    {
        return false;
    }
    void organizeBy(Common::Property::TypeID typeID) override
    {
        NXA_ALOG("This playlist can't be organized.");
    }
    void removeOrganizationBy(Common::Property::TypeID typeID) override
    {
        NXA_ALOG("This playlist can't be organized.");
    }
};

} }
