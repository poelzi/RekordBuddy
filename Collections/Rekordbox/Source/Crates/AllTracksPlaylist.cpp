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

#include <RekordboxCollection/Crates/AllTracksPlaylist.hpp>
#include <RekordboxCollection/Collection.hpp>

using namespace NxA;
using namespace NxA::Rekordbox;

// -- Constructors & Destructors

MutableAllTracksPlaylist::MutableAllTracksPlaylist(Pointer<MutableCollection> inCollection,
                                                   const MutableAllTracksPlaylist::Protected&) : p_collection{ inCollection } { }

// -- Instance Methods

Optional<count> MutableAllTracksPlaylist::p_addExistingTrackAtIndex(NotNull<Common::MutableTrack*> track, count toIndex)
{
    NXA_ASSERT_TRUE(this->collection() == track->collection());

    // -- If the track exists, that means it's already in the collection.
    // -- If it's in the collection that means creating it has added it to the AllTracksPlaylist already.
    // -- So we just check to see if it's already where it should be.
    auto indices = Common::Playlist::indicesForTrackIn(track, *this);
    NXA_ASSERT_TRUE_WITH_BLOCK(indices.length() == 1, [&indices, &track]() {
        CrashLog::addUserInfoWithKey(String::stringWithFormat("%llu", indices.length()), "nbOfIndicesFound");
        CrashLog::addUserInfoWithKey(track->absoluteFilePath().asEncodedString(), "absoluteFilePath");
    });

    (*this->p_collection).markAllTracksAsModifiedNow();

    // -- AllTrackPlaylist doesn't move an already existing track, it leaves it in place to preserve the user's ordering.
    return indices.firstObject();
}

NotNull<const Common::Collection*> MutableAllTracksPlaylist::collection() const
{
    return this->p_collection.asNotNull();
}

NotNull<Common::MutableCollection*> MutableAllTracksPlaylist::collection()
{
    return this->p_collection.asNotNull();
}

Time MutableAllTracksPlaylist::lastModificationTime() const
{
    return (*this->p_collection).lastAllTracksModificationTime();
}

count MutableAllTracksPlaylist::numberOfTracks() const
{
    return (*this->p_collection).numberOfTracks();
}

NotNull<const Common::Track*> MutableAllTracksPlaylist::trackAtIndex(count index) const
{
    return (*this->p_collection).trackAtIndex(index);
}

NotNull<Common::MutableTrack*> MutableAllTracksPlaylist::trackAtIndex(count index)
{
    return (*this->p_collection).trackAtIndex(index);
}

void MutableAllTracksPlaylist::moveTracksAtIndicesToIndex(Array<count> indices, count toIndex)
{
    (*this->p_collection).moveTracksAtIndicesToIndex(indices, toIndex);
}

void MutableAllTracksPlaylist::moveTrackAtIndexTo(count index, count toIndex)
{
    (*this->p_collection).moveTrackAtIndexTo(index, toIndex);
}

void MutableAllTracksPlaylist::removeTrackAtIndex(count index)
{
    auto& collection = (*this->p_collection);
    collection.rootFolder()->removeTrackWithAbsoluteFilePath(this->trackAtIndex(index)->absoluteFilePath());
    collection.removeTrackAtIndex(index);
}

void MutableAllTracksPlaylist::removeAllTracks()
{
    auto& collection = (*this->p_collection);
    collection.rootFolder()->removeAllTracks();
    collection.removeAllTracks();
}
