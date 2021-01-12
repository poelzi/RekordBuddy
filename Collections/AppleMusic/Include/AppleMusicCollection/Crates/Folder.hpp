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

#include <AppleMusicCollection/Tracks/Track.hpp>
#include <AppleMusicCollection/Crates/Playlist.hpp>

#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/CratePath.hpp>

#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>

namespace NxA { namespace AppleMusic {

// -- Forward Declarations
class Collection;
class Playlist;
class Folder;

// -- Public Interface
class Folder : public Common::Folder
{
    // -- Friends
    friend class Playlist;
    friend class Collection;

#if defined(NXA_BUILD_FOR_TESTING)
    friend class ITunesFolderTests;
    friend class ITunesPlaylistTests;
#endif

    // -- Private Types
    using SubCrate = Variant<Unique<Playlist>, Unique<Folder>>;

    // -- Private Instance Variables
    Pointer<Collection> p_collection;

    Optional<NotNull<const Folder*>> p_maybeParentFolder;

    String p_name;
    Array<String> p_persistentIDs;

    mutable Optional<Array<SubCrate>> p_maybeSubCrates;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Methods
    Array<SubCrate>& p_ensureSubCratesAreLoaded() const;

    inline Common::SubCrate p_asSubCrate() const
    {
        return { NotNull<const Common::Folder*>{ this } };
    }

public:
    // -- Constructors & Destructors
    Folder(String p_name,
           Array<String> persistentIDs,
           Pointer<Collection> inCollection,
           Optional<NotNull<const Folder*>> maybeWithParentFolder,
           const Protected&);
    ~Folder() override = default;

    // -- Operators
    template <class T>
        inline bool operator==(const T& other) const noexcept
        {
            return Common::Folder::isEqual(*this, other);
        }
    template <class T>
        inline bool operator!=(const T& other) const noexcept
        {
            return !Common::Folder::isEqual(*this, other);
        }
    template <class T>
        inline bool operator<(const T& other) const noexcept
        {
            return Common::Folder::isLessThan(*this, other);
        }

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

    Array<NotNull<const Common::Track*>> tracks() const override
    {
        return Folder::tracksIn<NotNull<const Common::Track*>>(*this);
    }

    count numberOfSubCrates() const override
    {
        return this->p_ensureSubCratesAreLoaded().length();
    }
    Common::SubCrate subCrateAtIndex(count) const override;
};

} }
