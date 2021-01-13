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

#include <PCDJCollection/Collection.hpp>
#include <PCDJCollection/Crates/Folder.hpp>

#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/SmartPlaylist.hpp>

using namespace NxA;
using namespace NxA::PCDJ;

// -- Constructors & Destructors

MutableFolder::MutableFolder(MutableXMLNode folder,
                             Pointer<MutableCollection> inCollection,
                             Optional<NotNull<MutableFolder*>> maybeWithParentFolder,
                             const Protected&) : p_collection{ inCollection },
                                                 p_maybeParentFolder{ maybeWithParentFolder },
                                                 p_lastModificationTime{ (*this->p_collection).lastPlaylistsModificationTime() },
                                                 p_pcdjFolder{ std::move(folder) } { }

// -- Instance Methods

MutableArray<MutableFolder::MutableSubCrate>& MutableFolder::p_ensureSubCratesAreLoaded() const
{
    if (!this->p_maybeSubNodes.isValid()) {
        this->p_maybeSubNodes = MutableArray<MutableSubCrate>{ };
        auto& subNodes = *this->p_maybeSubNodes;

        auto mutatedParentFolder = NotNull<MutableFolder*>{ const_cast<MutableFolder*>(this) };

        for (auto&& xmlNode : this->p_pcdjFolder.subNodesNamed("list")) {
            auto maybeType = xmlNode.maybeCountValueForAttributeNamed("type");
            if (!maybeType.isValid() || *maybeType == 200) {
                // -- 200 is the type of the Database playlist.
                continue;
            }

            subNodes.emplaceAppend(Unique<MutablePlaylist>::with(std::move(xmlNode),
                                                                 this->p_collection,
                                                                 mutatedParentFolder,
                                                                 PCDJ::MutablePlaylist::p_isProtected));
        }
    }

    return *this->p_maybeSubNodes;
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
    this->p_pcdjFolder.moveNodeNamedAtIndexTo("NODE", fromIndex, toIndex);

    this->p_markAsModifiedNow();
}

void MutableFolder::p_moveSubCrateToIndex(NotNull<Common::MutablePlaylist*> playlist, count index)
{
    auto maybeAsPCDJPlaylist = playlist.maybeAs<MutablePlaylist*>();
    NXA_ASSERT_TRUE(maybeAsPCDJPlaylist.isValid());

    auto& pcdjCrate = *maybeAsPCDJPlaylist;
    auto pcdjCratePointer = static_cast<void*>(pcdjCrate.get());
    auto otherParentFolder = pcdjCrate->p_parentFolder;
    auto& otherSubCrates = otherParentFolder->p_ensureSubCratesAreLoaded();
    count otherSubCrateIndex = 0;

    for (auto&& otherSubCrate : otherSubCrates) {
        if (otherSubCrate.apply([](auto&& crate) -> void* { return crate.asRawPointer(); }) == pcdjCratePointer) {
            break;
        }

        ++otherSubCrateIndex;
    }

    NXA_ASSERT_TRUE(otherSubCrateIndex < otherSubCrates.length());

    auto maybeSubNodeToMove = otherParentFolder->p_pcdjFolder.maybeSubNodeNamedAtIndex("NODE", otherSubCrateIndex);
    NXA_ASSERT_TRUE(maybeSubNodeToMove.isValid());

    this->p_pcdjFolder.addSubNodeAtIndex(*maybeSubNodeToMove, index);
    auto& thisSubCrates = this->p_ensureSubCratesAreLoaded();
    thisSubCrates.emplaceAt(thisSubCrates.begin() + index, otherSubCrates.removeAndReturnObjectAtIndex(otherSubCrateIndex));

    pcdjCrate->p_parentFolder = this;

    otherParentFolder->p_markAsModifiedNow();
    this->p_markAsModifiedNow();
}

void MutableFolder::p_moveSubCrateToIndex(NotNull<Common::MutableFolder*> folder, count index)
{
    auto maybeAsPCDJFolder = folder.maybeAs<MutableFolder*>();
    NXA_ASSERT_TRUE(maybeAsPCDJFolder.isValid());

    auto& pcdjCrate = *maybeAsPCDJFolder;
    auto pcdjCratePointer = static_cast<void*>(pcdjCrate.get());
    auto maybeOtherParentFolder = pcdjCrate->p_maybeParentFolder;
    NXA_ASSERT_TRUE(maybeOtherParentFolder.isValid());
    auto& otherParentFolder = *maybeOtherParentFolder;
    auto& otherSubCrates = otherParentFolder->p_ensureSubCratesAreLoaded();
    count otherSubCrateIndex = 0;

    for (auto&& otherSubCrate : otherSubCrates) {
        if (otherSubCrate.apply([](auto&& crate) -> void* { return crate.asRawPointer(); }) == pcdjCratePointer) {
            break;
        }

        ++otherSubCrateIndex;
    }

    NXA_ASSERT_TRUE(otherSubCrateIndex < otherSubCrates.length());

    auto maybeSubNodeToMove = otherParentFolder->p_pcdjFolder.maybeSubNodeNamedAtIndex("NODE", otherSubCrateIndex);
    NXA_ASSERT_TRUE(maybeSubNodeToMove.isValid());

    this->p_pcdjFolder.addSubNodeAtIndex(*maybeSubNodeToMove, index);
    this->p_ensureSubCratesAreLoaded().emplaceAppend(otherSubCrates.removeAndReturnObjectAtIndex(otherSubCrateIndex));

    pcdjCrate->p_maybeParentFolder = { this };

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

    auto currentNumberOfSubCrates = this->p_pcdjFolder.maybeCountValueForAttributeNamed("Count").valueOr(0);
    this->p_pcdjFolder.setCountValueForAttributeNamed(currentNumberOfSubCrates ? currentNumberOfSubCrates - 1 : 0, "Count");
    this->p_pcdjFolder.deleteSubNodeNamedAtIndex("NODE", index);

    subNodes.removeObjectAtIndex(index);

    this->p_markAsModifiedNow();
}

NotNull<Common::MutablePlaylist*> MutableFolder::newPlaylistWithName(const String& name)
{
    this->p_pcdjFolder.setCountValueForAttributeNamed(this->p_pcdjFolder.maybeCountValueForAttributeNamed("Count").valueOr(0) + 1, "Count");

    auto newNode = this->p_pcdjFolder.appendSubNodeNamed("NODE");
    newNode.setStringValueForAttributeNamed(this->nextAvailableNameForPlaylistNamed(name), "Name");
    newNode.setCountValueForAttributeNamed(1, "Type");
    newNode.setCountValueForAttributeNamed(0, "KeyType");
    newNode.setCountValueForAttributeNamed(0, "Entries");

    auto newPlaylist = Unique<PCDJ::MutablePlaylist>::with(std::move(newNode),
                                                                this->p_collection,
                                                                this,
                                                                PCDJ::MutablePlaylist::p_isProtected);
    auto asPointer = NotNull<Common::MutablePlaylist*>{ newPlaylist.asRawPointer() };

    this->p_ensureSubCratesAreLoaded().append(std::move(newPlaylist));

    this->p_markAsModifiedNow();

    return asPointer;
}

NotNull<Common::MutableFolder*> MutableFolder::newFolderWithName(const String& name)
{
    this->p_pcdjFolder.setCountValueForAttributeNamed(this->p_pcdjFolder.maybeCountValueForAttributeNamed("Count").valueOr(0) + 1, "Count");

    auto newNode = this->p_pcdjFolder.appendSubNodeNamed("NODE");
    newNode.setStringValueForAttributeNamed(this->nextAvailableNameForFolderNamed(name), "Name");
    newNode.setCountValueForAttributeNamed(0, "Type");
    newNode.setCountValueForAttributeNamed(0, "Count");

    auto newFolder = Unique<PCDJ::MutableFolder>::with(std::move(newNode),
                                                            this->p_collection,
                                                            NotNull<MutableFolder*>{ this },
                                                            PCDJ::MutableFolder::p_isProtected);
    auto asPointer = NotNull<Common::MutableFolder*>{ newFolder.asRawPointer() };

    this->p_ensureSubCratesAreLoaded().append(std::move(newFolder));

    this->p_markAsModifiedNow();

    return asPointer;
}
