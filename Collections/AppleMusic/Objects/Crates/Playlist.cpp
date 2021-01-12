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

#include <AppleMusicCollection/Crates/Playlist.hpp>
#include <AppleMusicCollection/Crates/Folder.hpp>
#include <AppleMusicCollection/Tracks/Track.hpp>
#include <AppleMusicCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::AppleMusic;

// -- Constructors & Destructors

Playlist::Playlist(String name,
                   XMLNode withPlaylist,
                   Pointer<Collection> inCollection,
                   NotNull<const Folder*> withParentFolder,
                   const Playlist::Protected&) : p_collection{ inCollection },
                                                 p_parentFolder{ withParentFolder },
                                                 p_name{ std::move(name) },
                                                 p_appleMusicPlaylist{ std::move(withPlaylist) } { }

// -- Instance Methods

Array<count>& Playlist::p_ensureTracksIdsAreLoaded() const
{
    if (!this->p_maybeTrackIDs.isValid()) {
        MutableArray<count> results;

        auto maybeNode = this->p_appleMusicPlaylist.maybeFirstSubNode();
        while (maybeNode.isValid()) {
            auto name = maybeNode->name();
            if (name == "key"_String) {
                auto maybeKey = maybeNode->maybeValue();
                if (maybeKey.isValid()) {
                    auto& key = *maybeKey;

                    auto maybeSubNode = maybeNode->maybeSiblingNode();
                    if (maybeSubNode.isValid()) {
                        if ((key == "Playlist Items"_String) && (maybeSubNode->name() == "array"_String)) {
                            auto maybeTrackDict = maybeSubNode->maybeFirstSubNode();
                            while (maybeTrackDict.isValid()) {
                                auto maybeTrackKey = maybeTrackDict->maybeFirstSubNode();
                                if (maybeTrackKey.isValid() && (maybeTrackKey->name() == "key"_String)) {
                                    auto maybeTrackKeyValue = maybeTrackKey->maybeValue();
                                    if (maybeTrackKeyValue.isValid() && (*maybeTrackKeyValue == "Track ID"_String)) {
                                        auto maybeTrackIDNode = maybeTrackKey->maybeSiblingNode();
                                        if (maybeTrackIDNode.isValid() && (maybeTrackIDNode->name() == "integer"_String)) {
                                            auto maybeTrackID = maybeTrackIDNode->maybeValue();
                                            if (maybeTrackID.isValid()) {
                                                auto trackIDAsInteger = maybeTrackID->integerValue();
                                                auto maybeTrack = this->p_collection.asReference().maybeExistingTrackForTrackID(trackIDAsInteger);
                                                if (maybeTrack.isValid()) {
                                                    results.append(trackIDAsInteger);
                                                }
                                            }
                                        }
                                    }
                                }

                                maybeTrackDict = maybeTrackDict->maybeSiblingNode();
                            }
                        }
                    }
                }
            }

            maybeNode = maybeNode->maybeSiblingNode();
        }

        this->p_maybeTrackIDs = Array<count>{ std::move(results) };
    }

    return *this->p_maybeTrackIDs;
}

NotNull<const Common::Collection*> Playlist::collection() const
{
    return this->p_collection.asNotNull();
}

Optional<NotNull<const Common::Folder*>> Playlist::maybeParentFolder() const
{
    return { this->p_parentFolder.as<const Common::Folder*>() };
}

Time Playlist::lastModificationTime() const
{
    return this->p_collection.asReference().lastModificationTime();
}

NotNull<const Common::Track*> Playlist::trackAtIndex(count index) const
{
    auto trackID = this->p_ensureTracksIdsAreLoaded()[index];

    auto maybeTrack = this->p_collection.asReference().maybeExistingTrackForTrackID(trackID);
    NXA_ASSERT_TRUE(maybeTrack.isValid());

    return *maybeTrack;
}
