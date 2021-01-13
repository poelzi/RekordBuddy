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

#include <PCDJCollection/Crates/Playlist.hpp>
#include <PCDJCollection/Crates/Folder.hpp>
#include <PCDJCollection/Tracks/Track.hpp>
#include <PCDJCollection/Collection.hpp>

#include <TrackFiles/TrackFile.hpp>

using namespace NxA;
using namespace NxA::PCDJ;

// -- Constructors & Destructors

MutablePlaylist::MutablePlaylist(MutableXMLNode withPlaylist,
                                 Pointer<MutableCollection> inCollection,
                                 NotNull<MutableFolder*> withParentFolder,
                                 const MutablePlaylist::Protected&) : p_collection{ inCollection },
                                                                      p_parentFolder{ withParentFolder },
                                                                      p_lastModificationTime{ (*this->p_collection).lastPlaylistsModificationTime() },
                                                                      p_pcdjPlaylist{ std::move(withPlaylist) } { }

// -- Instance Methods

Optional<NotNull<const Common::Track*>> MutablePlaylist::p_maybeTrackForNode(const MutableXMLNode& xmlNode) const
{
    auto maybeTrackID = xmlNode.maybeCountValueForAttributeNamed("id");
    if (maybeTrackID.isValid()) {
        return (*this->p_collection).maybeExistingTrackForTrackID(*maybeTrackID);
    }

    return nothing;
}

Optional<NotNull<Common::MutableTrack*>> MutablePlaylist::p_maybeTrackForNode(const MutableXMLNode& xmlNode)
{
    auto maybeTrackID = xmlNode.maybeCountValueForAttributeNamed("id");
    if (maybeTrackID.isValid()) {
        return (*this->p_collection).maybeExistingTrackForTrackID(*maybeTrackID);
    }

    return nothing;
}

MutableArray<MutableXMLNode>& MutablePlaylist::p_ensureTracksAreLoaded() const
{
    if (!this->p_maybeSubNodes.isValid()) {
        auto mutatedThis = const_cast<MutablePlaylist*>(this);

        this->p_maybeSubNodes = MutableArray<MutableXMLNode>{ };
        auto& subNodesFound = *(this->p_maybeSubNodes);
        count index = 0;

        for (auto& subNode : mutatedThis->p_pcdjPlaylist.subNodesNamed("item")) {
            // -- The method we are in is const but it's really mutable because its used by both const and non const methods behind the scenes.
            if (const_cast<MutablePlaylist*>(this)->p_maybeTrackForNode(subNode).isValid()) {
                subNodesFound.append(std::move(subNode));

                ++index;
            }
        }
    }

    return *this->p_maybeSubNodes;
}

void MutablePlaylist::p_markAsModifiedNow()
{
    this->p_lastModificationTime = Time::currentTime();
    (*this->p_collection).markPlaylistsAsModifiedNow();
}

Optional<count> MutablePlaylist::p_addExistingTrackAtIndex(NotNull<Common::MutableTrack*> track, count toIndex)
{
    if (Common::Track::isAMovieTrack(*track) && !this->collection()->allowsMovieTracksInPlaylists()) {
        return nothing;
    }

    auto maybePCDJTrack = track.maybeAs<PCDJ::MutableTrack*>();
    NXA_ASSERT_TRUE(maybePCDJTrack.isValid());
    NXA_ASSERT_TRUE(this->collection() == track->collection());

    // -- We have to ensure the subnodes are loaded before we add the new subnode, otherwise we would end up adding it twice.
    auto& subNodes = this->p_ensureTracksAreLoaded();

    auto& playlistNode = this->p_pcdjPlaylist;
    auto newTrackNode = playlistNode.appendSubNodeNamed("item");

    auto maybeTrackID = (*maybePCDJTrack)->maybeTrackID();
    NXA_ASSERT_TRUE(maybeTrackID.isValid());

    newTrackNode.setCountValueForAttributeNamed(*maybeTrackID, "id");

    subNodes.append(newTrackNode);

    auto currentIndex = subNodes.length() - 1;
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
    this->p_pcdjPlaylist.setStringValueForAttributeNamed(this->p_parentFolder->nextAvailableNameForPlaylistNamed(name), "name");

    this->p_markAsModifiedNow();
}

NotNull<const Common::Track*> MutablePlaylist::trackAtIndex(count index) const
{
    auto maybeTrack = this->p_maybeTrackForNode(this->p_ensureTracksAreLoaded()[index]);
    NXA_ASSERT_TRUE(maybeTrack.isValid());

    return *maybeTrack;
}

NotNull<Common::MutableTrack*> MutablePlaylist::trackAtIndex(count index)
{
    auto maybeTrack = this->p_maybeTrackForNode(this->p_ensureTracksAreLoaded()[index]);
    NXA_ASSERT_TRUE(maybeTrack.isValid());

    return *maybeTrack;
}

void MutablePlaylist::moveTracksAtIndicesToIndex(Array<count> indices, count toIndex)
{
    this->p_ensureTracksAreLoaded().moveObjectsAtIndicesTo(indices, toIndex);
    this->p_pcdjPlaylist.moveNodesNamedAtIndicesToIndex("item", indices, toIndex);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::moveTrackAtIndexTo(count index, count toIndex)
{
    this->p_ensureTracksAreLoaded().moveObjectAtIndexTo(index, toIndex);
    this->p_pcdjPlaylist.moveNodeNamedAtIndexTo("item", index, toIndex);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::removeTrackAtIndex(count index)
{
    this->p_ensureTracksAreLoaded().removeObjectAtIndex(index);
    this->p_pcdjPlaylist.deleteSubNodeNamedAtIndex("item", index);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::removeAllTracks()
{
    this->p_maybeSubNodes = nothing;
    this->p_pcdjPlaylist.deleteSubNodesNamed("item");

    this->p_markAsModifiedNow();
}
