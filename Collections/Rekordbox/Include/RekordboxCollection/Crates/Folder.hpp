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

#include <RekordboxCollection/Tracks/Track.hpp>
#include <RekordboxCollection/Crates/Playlist.hpp>

#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/CratePath.hpp>

#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>

namespace NxA { namespace Rekordbox {

// -- Forward Declarations
class MutableCollection;
class MutablePlaylist;
class MutableFolder;

// -- Public Interface
class MutableFolder : public Common::MutableFolder, public Common::Folder
{
    // -- Friends
    friend class MutablePlaylist;
    friend class MutableCollection;

#if defined(NXA_BUILD_FOR_TESTING)
    friend class RekordboxFolderTests;
    friend class RekordboxPlaylistTests;
#endif

    // -- Private Types
    using MutableSubCrate = Variant<Unique<MutablePlaylist>, Unique<MutableFolder>>;

    // -- Private Instance Variables
    Pointer<MutableCollection> p_collection;

    Optional<NotNull<MutableFolder*>> p_maybeParentFolder;

    Time p_lastModificationTime;

    mutable Optional<MutableArray<MutableSubCrate>> p_maybeSubNodes;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Variables
    mutable MutableXMLNode p_rekordboxFolder;

    // -- Protected Instance Methods
    MutableArray<MutableSubCrate>& p_ensureSubCratesAreLoaded() const;

    void p_markAsModifiedNow();

    void p_moveSubCrateAtIndexToIndex(count, count) override;

    inline Common::SubCrate p_asSubCrate() const
    {
        return { NotNull<const Common::Folder*>{ this } };
    }
    inline Common::MutableSubCrate p_asSubCrate()
    {
        return { NotNull<Common::MutableFolder*>{ this } };
    }

    void p_moveSubCrateToIndex(NotNull<Common::MutablePlaylist*>, count) override;
    void p_moveSubCrateToIndex(NotNull<Common::MutableFolder*>, count) override;

public:
    // -- Constructors & Destructors
    MutableFolder(MutableXMLNode folder,
                  Pointer<MutableCollection> inCollection,
                  Optional<NotNull<MutableFolder*>> maybeWithParentFolder,
                  const Protected&);
    ~MutableFolder() override = default;

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
    NotNull<Common::MutableCollection*> collection() override;

    Optional<NotNull<const Common::Folder*>> maybeParentFolder() const override;
    Optional<NotNull<Common::MutableFolder*>> maybeParentFolder() override;

    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
    }
    String name() const override
    {
        return this->p_rekordboxFolder.maybeStringValueForAttributeNamed("Name").valueOr("<No Name>"_String);
    }
    void setName(const String& name) override
    {
        if (name == this->name()) {
            return;
        }

        auto maybeParentFolder = this->p_maybeParentFolder;
        NXA_ASSERT_TRUE(maybeParentFolder.isValid());

        this->p_markAsModifiedNow();
        this->p_rekordboxFolder.setStringValueForAttributeNamed((*maybeParentFolder)->nextAvailableNameForFolderNamed(name), "Name");
    }
    inline Common::CratePath path() const override
    {
        return Common::Folder::cratePathFor(*this);
    }

    Array<NotNull<const Common::Track*>> tracks() const override
    {
        return Folder::tracksIn<NotNull<const Common::Track*>>(*this);
    }
    Array<NotNull<Common::MutableTrack*>> tracks() override
    {
        return Folder::tracksIn<NotNull<Common::MutableTrack*>>(*this);
    }

    count numberOfSubCrates() const override
    {
        return this->p_ensureSubCratesAreLoaded().length();
    }
    Common::SubCrate subCrateAtIndex(count) const override;
    Common::MutableSubCrate subCrateAtIndex(count) override;
    void removeSubCrateAtIndex(count) override;

    NotNull<Common::MutablePlaylist*> newPlaylistWithName(const String&) override;
    NotNull<Common::MutableFolder*> newFolderWithName(const String&) override;

    void renumberTrackIDsWith(const Map<count, count>&);
};

} }
