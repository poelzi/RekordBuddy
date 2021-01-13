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

#include <StubCollection/Crates/Playlist.hpp>
#include <StubCollection/Crates/Folder.hpp>
#include <StubCollection/Tracks/Track.hpp>
#include <StubCollection/Collection.hpp>

#include <TrackFiles/TrackFile.hpp>

using namespace NxA;
using namespace NxA::Stub;

// -- Constructors & Destructors

MutablePlaylist::MutablePlaylist(Pointer<MutableCollection> inCollection,
                                 NotNull<MutableFolder*> withParentFolder,
                                 const MutablePlaylist::Protected&) : p_collection{ inCollection },
                                                                      p_parentFolder{ withParentFolder },
                                                                      p_lastModificationTime{ (*this->p_collection).lastPlaylistsModificationTime() } { }

// -- Instance Methods

MutableArray<Shared<MutableTrack>>& MutablePlaylist::p_ensureTracksAreLoaded() const
{
    // -- To be implemented.
    static MutableArray<Shared<MutableTrack>> stub;
    return stub;
}

void MutablePlaylist::p_markAsModifiedNow()
{
    this->p_lastModificationTime = Time::currentTime();
    (*this->p_collection).markPlaylistsAsModifiedNow();
}

Optional<count> MutablePlaylist::p_addExistingTrackAtIndex(NotNull<Common::MutableTrack*> track, count toIndex)
{
    auto maybeStubTrack = track.maybeAs<Stub::MutableTrack*>();
    NXA_ASSERT_TRUE(maybeStubTrack.isValid());
    NXA_ASSERT_TRUE(this->collection() == track->collection());

    // -- We have to ensure the tracks are loaded before we add the new track, otherwise we would end up adding it twice.
    auto currentIndex = this->p_ensureTracksAreLoaded().length() - 1;
    if (toIndex == currentIndex) {
        this->p_markAsModifiedNow();
        return toIndex;
    }

    count resultingIndex = currentIndex < toIndex ? (toIndex - 1) : toIndex;
    this->moveTrackAtIndexTo(currentIndex, toIndex);

    return resultingIndex;
}

NotNull<const Common::Collection*> MutablePlaylist::collection() const
{
    return this->p_collection.asNotNull();
}

NotNull<Common::MutableCollection*> MutablePlaylist::collection()
{
    return this->p_collection.asNotNull();
}

Optional<NotNull<const Common::Folder*>> MutablePlaylist::maybeParentFolder() const
{
    return { this->p_parentFolder.as<const Common::Folder*>() };
}

Optional<NotNull<Common::MutableFolder*>> MutablePlaylist::maybeParentFolder()
{
    return { this->p_parentFolder.as<Common::MutableFolder*>() };
}

void MutablePlaylist::setName(const String& name)
{

    this->p_markAsModifiedNow();
}

NotNull<const Common::Track*> MutablePlaylist::trackAtIndex(count index) const
{
    return { this->p_ensureTracksAreLoaded()[index].asRawPointer() };
}

NotNull<Common::MutableTrack*> MutablePlaylist::trackAtIndex(count index)
{
    return { this->p_ensureTracksAreLoaded()[index].asRawPointer() };
}

void MutablePlaylist::moveTracksAtIndicesToIndex(Array<count> indices, count toIndex)
{
    this->p_ensureTracksAreLoaded().moveObjectsAtIndicesTo(indices, toIndex);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::moveTrackAtIndexTo(count index, count toIndex)
{
    this->p_ensureTracksAreLoaded().moveObjectAtIndexTo(index, toIndex);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::removeTrackAtIndex(count index)
{
    this->p_ensureTracksAreLoaded().removeObjectAtIndex(index);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::removeAllTracks()
{
    // -- To be implemented

    this->p_markAsModifiedNow();
}
