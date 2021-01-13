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

#include "TraktorCollection/Collection.hpp"
#include <RekordBuddyCollection/Crates/Crates.hpp>

#include <CommonCollection/Crates/SubCrate.hpp>
#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/SmartPlaylist.hpp>

using namespace NxA;
using namespace Traktor;

// -- Constructors & Destructors

MutableFolder::MutableFolder(MutableXMLNode withFolder,
                             Pointer<MutableCollection> collection,
                             Optional<NotNull<MutableFolder*>> maybeWithParentFolder,
                             const Protected&) : p_collection{ collection },
                                                 p_maybeParentFolder{ maybeWithParentFolder },
                                                 p_lastModificationTime{ (*this->p_collection).lastPlaylistsModificationTime() },
                                                 p_traktorFolder{ std::move(withFolder) } { }

// -- Instance Methods

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
    this->p_subNodesNode().moveNodeNamedAtIndexTo("NODE", fromIndex, toIndex);

    this->p_markAsModifiedNow();
}

MutableArray<MutableFolder::MutableSubCrate>& MutableFolder::p_ensureSubCratesAreLoaded() const
{
    if (!this->p_maybeSubNodes.isValid()) {
        this->p_maybeSubNodes = MutableArray<MutableSubCrate>{ };
        auto& subNodes = *this->p_maybeSubNodes;

        auto mutatedParentFolder = NotNull<MutableFolder*>{ const_cast<MutableFolder*>(this) };

        for (auto&& xmlNode : this->p_subNodesNode().subNodesNamed("NODE")) {
            auto maybeNodeType = xmlNode.maybeStringValueForAttributeNamed("TYPE");
            NXA_ASSERT_TRUE(maybeNodeType.isValid());

            if (*maybeNodeType == "PLAYLIST"_String) {
                subNodes.emplaceAppend(Unique<Traktor::MutablePlaylist>::with(std::move(xmlNode),
                                                                              this->p_collection,
                                                                              mutatedParentFolder,
                                                                              Traktor::MutablePlaylist::p_isProtected));
            }
            else if (*maybeNodeType == "FOLDER"_String) {
                subNodes.emplaceAppend(Unique<Traktor::MutableFolder>::with(std::move(xmlNode),
                                                                            this->p_collection,
                                                                            mutatedParentFolder,
                                                                            Traktor::MutableFolder::p_isProtected));
            }
        }
    }

    return *this->p_maybeSubNodes;
}

void MutableFolder::p_moveSubCrateToIndex(NotNull<Common::MutablePlaylist*> playlist, count index)
{
    auto maybeAsTraktorPlaylist = playlist.maybeAs<MutablePlaylist*>();
    NXA_ASSERT_TRUE(maybeAsTraktorPlaylist.isValid());

    auto& traktorCrate = *maybeAsTraktorPlaylist;
    auto traktorCratePointer = static_cast<void*>(traktorCrate.get());
    auto otherParentFolder = traktorCrate->p_parentFolder;
    auto& otherSubCrates = otherParentFolder->p_ensureSubCratesAreLoaded();
    count otherSubCrateIndex = 0;

    for (auto&& otherSubCrate : otherSubCrates) {
        if (otherSubCrate.apply([](auto&& crate) -> void* { return crate.asRawPointer(); }) == traktorCratePointer) {
            break;
        }

        ++otherSubCrateIndex;
    }

    NXA_ASSERT_TRUE(otherSubCrateIndex < otherSubCrates.length());

    auto otherSubNodesNode = otherParentFolder->p_subNodesNode();
    auto maybeSubNodeToMove = otherSubNodesNode.maybeSubNodeNamedAtIndex("NODE", otherSubCrateIndex);
    NXA_ASSERT_TRUE(maybeSubNodeToMove.isValid());

    this->p_subNodesNode().addSubNodeAtIndex(*maybeSubNodeToMove, index);
    auto& thisSubCrates = this->p_ensureSubCratesAreLoaded();
    thisSubCrates.emplaceAt(thisSubCrates.begin() + index, otherSubCrates.removeAndReturnObjectAtIndex(otherSubCrateIndex));

    traktorCrate->p_parentFolder = this;

    otherParentFolder->p_markAsModifiedNow();
    this->p_markAsModifiedNow();
}

void MutableFolder::p_moveSubCrateToIndex(NotNull<Common::MutableFolder*> folder, count index)
{
    auto maybeAsTraktorFolder = folder.maybeAs<MutableFolder*>();
    NXA_ASSERT_TRUE(maybeAsTraktorFolder.isValid());

    auto& traktorCrate = *maybeAsTraktorFolder;
    auto traktorCratePointer = static_cast<void*>(traktorCrate.get());
    auto maybeOtherParentFolder = traktorCrate->p_maybeParentFolder;
    NXA_ASSERT_TRUE(maybeOtherParentFolder.isValid());
    auto& otherParentFolder = *maybeOtherParentFolder;
    auto& otherSubCrates = otherParentFolder->p_ensureSubCratesAreLoaded();
    count otherSubCrateIndex = 0;

    for (auto&& otherSubCrate : otherSubCrates) {
        if (otherSubCrate.apply([](auto&& crate) -> void* { return crate.asRawPointer(); }) == traktorCratePointer) {
            break;
        }

        ++otherSubCrateIndex;
    }

    NXA_ASSERT_TRUE(otherSubCrateIndex < otherSubCrates.length());

    auto otherSubNodesNode = otherParentFolder->p_subNodesNode();
    auto maybeSubNodeToMove = otherSubNodesNode.maybeSubNodeNamedAtIndex("NODE", otherSubCrateIndex);
    NXA_ASSERT_TRUE(maybeSubNodeToMove.isValid());

    this->p_subNodesNode().addSubNodeAtIndex(*maybeSubNodeToMove, index);
    this->p_ensureSubCratesAreLoaded().emplaceAppend(otherSubCrates.removeAndReturnObjectAtIndex(otherSubCrateIndex));

    traktorCrate->p_maybeParentFolder = { this };

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

    auto subNodesNode = this->p_subNodesNode();
    subNodesNode.setCountValueForAttributeNamed(subNodesNode.maybeCountValueForAttributeNamed("COUNT").valueOr(subNodes.length()) - 1, "COUNT");
    subNodesNode.deleteSubNodeNamedAtIndex("NODE", index);

    subNodes.removeObjectAtIndex(index);

    this->p_markAsModifiedNow();
}

NotNull<Common::MutablePlaylist*> MutableFolder::newPlaylistWithName(const String& name)
{
    auto subNodesNode = this->p_subNodesNode();
    subNodesNode.setCountValueForAttributeNamed(subNodesNode.maybeCountValueForAttributeNamed("COUNT").valueOr(0) + 1, "COUNT");

    auto newNode = subNodesNode.appendSubNodeNamed("NODE");
    newNode.setStringValueForAttributeNamed("PLAYLIST"_String, "TYPE");
    newNode.setStringValueForAttributeNamed(this->nextAvailableNameForPlaylistNamed(name), "NAME");

    auto newPlaylistNode = newNode.appendSubNodeNamed("PLAYLIST");
    newPlaylistNode.setCountValueForAttributeNamed(0, "ENTRIES");
    newPlaylistNode.setStringValueForAttributeNamed("LIST"_String, "TYPE");

    auto newPlaylist = Unique<Traktor::MutablePlaylist>::with(std::move(newNode),
                                                              this->p_collection,
                                                              this,
                                                              Traktor::MutablePlaylist::p_isProtected);
    auto asPointer = NotNull<Common::MutablePlaylist*>{ newPlaylist.asRawPointer() };

    this->p_ensureSubCratesAreLoaded().append(std::move(newPlaylist));

    this->p_markAsModifiedNow();

    return asPointer;
}

NotNull<Common::MutableFolder*> MutableFolder::newFolderWithName(const String& name)
{
    auto subNodesNode = this->p_subNodesNode();
    subNodesNode.setCountValueForAttributeNamed(subNodesNode.maybeCountValueForAttributeNamed("COUNT").valueOr(0) + 1, "COUNT");

    auto newNode = subNodesNode.appendSubNodeNamed("NODE");
    newNode.setStringValueForAttributeNamed("FOLDER"_String, "TYPE");
    newNode.setStringValueForAttributeNamed(this->nextAvailableNameForFolderNamed(name), "NAME");

    auto newFolder = Unique<Traktor::MutableFolder>::with(std::move(newNode),
                                                          this->p_collection,
                                                          Optional<NotNull<MutableFolder*>>{ this },
                                                          Traktor::MutableFolder::p_isProtected);
    auto asPointer = NotNull<Common::MutableFolder*>{ newFolder.asRawPointer() };

    this->p_ensureSubCratesAreLoaded().append(std::move(newFolder));

    this->p_markAsModifiedNow();

    return asPointer;
}
