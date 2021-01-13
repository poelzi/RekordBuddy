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

#include <EngineCollection/Collection.hpp>
#include <EngineCollection/Crates/Folder.hpp>

#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/SmartPlaylist.hpp>

using namespace NxA;
using namespace NxA::Engine;

// -- Constructors & Destructors

MutableFolder::MutableFolder(Pointer<MutableCollection> inCollection,
                             Optional<NotNull<MutableFolder*>> maybeWithParentFolder,
                             const Protected&) : p_collection{ inCollection },
                                                 p_maybeParentFolder{ maybeWithParentFolder },
                                                 p_lastModificationTime{ (*this->p_collection).lastPlaylistsModificationTime() } { }

// -- Instance Methods

MutableArray<MutableFolder::MutableSubCrate>& MutableFolder::p_ensureSubCratesAreLoaded() const
{
    if (!this->p_maybeSubCrates.isValid()) {
        this->p_maybeSubCrates = MutableArray<MutableSubCrate>{ };

        //auto& subCrates = *this->p_maybeSubCrates;
        //auto mutatedParentFolder = NotNull<MutableFolder*>{ const_cast<MutableFolder*>(this) };

        // -- TODO
    }

    return *this->p_maybeSubCrates;
}

void MutableFolder::p_markAsModifiedNow()
{
    this->p_lastModificationTime = Time::currentTime();
    (*this->p_collection).markPlaylistsAsModifiedNow();
}

void MutableFolder::p_moveSubCrateAtIndexToIndex(count fromIndex, count toIndex)
{
    if (fromIndex == toIndex) {
        return;
    }

    this->p_ensureSubCratesAreLoaded().moveObjectAtIndexTo(fromIndex, toIndex);

    this->p_markAsModifiedNow();
}

void MutableFolder::p_moveSubCrateToIndex(NotNull<Common::MutablePlaylist*> playlist, count index)
{
    auto maybeAsEnginePlaylist = playlist.maybeAs<MutablePlaylist*>();
    NXA_ASSERT_TRUE(maybeAsEnginePlaylist.isValid());

    auto& asEngineCrate = *maybeAsEnginePlaylist;
    auto asEngineCratePointer = static_cast<void*>(asEngineCrate.get());
    auto otherParentFolder = asEngineCrate->p_parentFolder;
    auto& otherSubCrates = otherParentFolder->p_ensureSubCratesAreLoaded();
    count otherSubCrateIndex = 0;

    for (auto&& otherSubCrate : otherSubCrates) {
        if (otherSubCrate.apply([](auto&& crate) -> void* { return crate.asRawPointer(); }) == asEngineCratePointer) {
            break;
        }

        ++otherSubCrateIndex;
    }

    NXA_ASSERT_TRUE(otherSubCrateIndex < otherSubCrates.length());

    auto& thisSubCrates = this->p_ensureSubCratesAreLoaded();
    thisSubCrates.emplaceAt(thisSubCrates.begin() + index, otherSubCrates.removeAndReturnObjectAtIndex(otherSubCrateIndex));

    asEngineCrate->p_parentFolder = this;

    otherParentFolder->p_markAsModifiedNow();
    this->p_markAsModifiedNow();
}

void MutableFolder::p_moveSubCrateToIndex(NotNull<Common::MutableFolder*> folder, count index)
{
    auto maybeAsEngineFolder = folder.maybeAs<MutableFolder*>();
    NXA_ASSERT_TRUE(maybeAsEngineFolder.isValid());

    auto& asEngineCrate = *maybeAsEngineFolder;
    auto asEngineCratePointer = static_cast<void*>(asEngineCrate.get());
    auto maybeOtherParentFolder = asEngineCrate->p_maybeParentFolder;
    NXA_ASSERT_TRUE(maybeOtherParentFolder.isValid());
    auto& otherParentFolder = *maybeOtherParentFolder;
    auto& otherSubCrates = otherParentFolder->p_ensureSubCratesAreLoaded();
    count otherSubCrateIndex = 0;

    for (auto&& otherSubCrate : otherSubCrates) {
        if (otherSubCrate.apply([](auto&& crate) -> void* { return crate.asRawPointer(); }) == asEngineCratePointer) {
            break;
        }

        ++otherSubCrateIndex;
    }

    NXA_ASSERT_TRUE(otherSubCrateIndex < otherSubCrates.length());

    this->p_ensureSubCratesAreLoaded().emplaceAppend(otherSubCrates.removeAndReturnObjectAtIndex(otherSubCrateIndex));

    asEngineCrate->p_maybeParentFolder = { this };

    otherParentFolder->p_markAsModifiedNow();
    this->p_markAsModifiedNow();
}

NotNull<const Common::Collection*> MutableFolder::collection() const
{
    return this->p_collection.asNotNull();
}

NotNull<Common::MutableCollection*> MutableFolder::collection()
{
    return this->p_collection.asNotNull();
}

Optional<NotNull<const Common::Folder*>> MutableFolder::maybeParentFolder() const
{
    return this->p_maybeParentFolder.maybe([](auto folder) {
        return folder.template as<const Common::Folder*>();
    });
}

Optional<NotNull<Common::MutableFolder*>> MutableFolder::maybeParentFolder()
{
    return this->p_maybeParentFolder.maybe([](auto folder) {
        return folder.template as<Common::MutableFolder*>();
    });
}

Common::SubCrate MutableFolder::subCrateAtIndex(count index) const
{
    return this->p_ensureSubCratesAreLoaded()[index].apply([](const auto& crate) {
        return crate->p_asSubCrate();
    });
}

Common::MutableSubCrate MutableFolder::subCrateAtIndex(count index)
{
    return this->p_ensureSubCratesAreLoaded()[index].apply([](auto& crate) {
        return crate->p_asSubCrate();
    });
}

void MutableFolder::removeSubCrateAtIndex(count index)
{
    auto& subNodes = this->p_ensureSubCratesAreLoaded();

    subNodes.removeObjectAtIndex(index);

    this->p_markAsModifiedNow();
}

NotNull<Common::MutablePlaylist*> MutableFolder::newPlaylistWithName(const String& name)
{
    auto newPlaylist = Unique<Engine::MutablePlaylist>::with(this->p_collection,
                                                             this,
                                                             Engine::MutablePlaylist::p_isProtected);
    auto asPointer = NotNull<Common::MutablePlaylist*>{ newPlaylist.asRawPointer() };

    this->p_ensureSubCratesAreLoaded().append(std::move(newPlaylist));

    this->p_markAsModifiedNow();

    return asPointer;
}

NotNull<Common::MutableFolder*> MutableFolder::newFolderWithName(const String& name)
{
    auto newFolder = Unique<Engine::MutableFolder>::with(this->p_collection,
                                                         NotNull<MutableFolder*>{ this },
                                                         Engine::MutableFolder::p_isProtected);
    auto asPointer = NotNull<Common::MutableFolder*>{ newFolder.asRawPointer() };

    this->p_ensureSubCratesAreLoaded().append(std::move(newFolder));

    this->p_markAsModifiedNow();

    return asPointer;
}
