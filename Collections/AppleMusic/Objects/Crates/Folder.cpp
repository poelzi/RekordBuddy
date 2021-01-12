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

#include <AppleMusicCollection/Crates/Folder.hpp>
#include <AppleMusicCollection/Collection.hpp>

#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/SmartPlaylist.hpp>

using namespace NxA;
using namespace NxA::AppleMusic;

// -- Constructors & Destructors

Folder::Folder(String name,
               Array<String> persistentIDs,
               Pointer<Collection> inCollection,
               Optional<NotNull<const Folder*>> maybeWithParentFolder,
               const Protected&) : p_collection{ inCollection },
                                   p_maybeParentFolder{ maybeWithParentFolder },
                                   p_name{ std::move(name) },
                                   p_persistentIDs{ persistentIDs } { }

// -- Instance Methods

Array<Folder::SubCrate>& Folder::p_ensureSubCratesAreLoaded() const
{
    if (!this->p_maybeSubCrates.isValid()) {
        MutableArray<SubCrate> results;

        for (auto&& persistentId : this->p_persistentIDs) {
            auto name = this->p_collection.asReference().maybePlaylistNameForPersistentID(persistentId).valueOr("<No Name>"_String);
            auto maybeChildren = this->p_collection.asReference().maybeChildrenForPersistentID(persistentId);

            if (!maybeChildren.isValid()) {
                auto maybePlaylist = this->p_collection.asReference().maybePlaylistForPersistentID(persistentId);
                if (maybePlaylist.isValid()) {
                    results.emplaceAppend(Unique<Playlist>::with(name,
                                                                 *maybePlaylist,
                                                                 this->p_collection,
                                                                 NotNull<const Folder*>{ this },
                                                                 AppleMusic::Playlist::p_isProtected));
                }
            }
            else {
                results.emplaceAppend(Unique<Folder>::with(name,
                                                           *maybeChildren,
                                                           this->p_collection,
                                                           NotNull<const Folder*>{ this },
                                                           AppleMusic::Folder::p_isProtected));
            }
        }

        this->p_maybeSubCrates = { std::move(results) };
    }

    return *this->p_maybeSubCrates;
}

NotNull<const Common::Collection*> Folder::collection() const
{
    return this->p_collection.asNotNull();
}

Optional<NotNull<const Common::Folder*>> Folder::maybeParentFolder() const
{
    return this->p_maybeParentFolder.maybe([](auto folder) {
        return folder.template as<const Common::Folder*>();
    });
}

Time Folder::lastModificationTime() const
{
    return this->p_collection.asReference().lastModificationTime();
}

Common::SubCrate Folder::subCrateAtIndex(count index) const
{
    return this->p_ensureSubCratesAreLoaded()[index].apply([](const auto& crate) {
        return crate->p_asSubCrate();
    });
}
