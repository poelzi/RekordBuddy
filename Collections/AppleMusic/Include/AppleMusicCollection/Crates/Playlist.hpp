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

#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/CratePath.hpp>
#include <CommonCollection/Tracks/Track.hpp>

#include <Base/NotNull.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>

namespace NxA { namespace AppleMusic {

// -- Forward Declarations
class Collection;
class Folder;

// -- Internal Interface
class Playlist : public Common::Playlist
{
    // -- Friends
    friend class Folder;

#if defined(NXA_BUILD_FOR_TESTING)
    friend class ITunesPlaylistTests;
#endif

    // -- Private Instance Variables
    Pointer<Collection> p_collection;

    NotNull<const Folder*> p_parentFolder;

    String p_name;
    XMLNode p_appleMusicPlaylist;

    mutable Optional<Array<count>> p_maybeTrackIDs;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    Array<count>& p_ensureTracksIdsAreLoaded() const;

    inline Common::SubCrate p_asSubCrate() const
    {
        return { NotNull<const Common::Playlist*>{ this } };
    }

public:
    // -- Constructors & Destructors
    Playlist(String name,
             XMLNode withPlaylist,
             Pointer<Collection> inCollection,
             NotNull<const Folder*> withParentFolder,
             const Protected&);
    ~Playlist() override = default;

    // -- Instance Methods
    NotNull<const Common::Collection*> collection() const override;

    Optional<NotNull<const Common::Folder*>> maybeParentFolder() const override;

    Time lastModificationTime() const override;
    String name() const override
    {
        return this->p_name;
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

    count numberOfTracks() const override
    {
        return this->p_ensureTracksIdsAreLoaded().length();
    }
    NotNull<const Common::Track*> trackAtIndex(count) const override;
    Array<NotNull<const Common::Track*>> tracks() const override
    {
        return Common::Playlist::tracksIn<NotNull<const Common::Track*>>(*this);
    }

    inline boolean isOrganizedBy(const Common::Tag&) const override
    {
        return false;
    }
    inline boolean isOrganizedBy(Common::Property::TypeID) const override
    {
        return false;
    }
};

} }
