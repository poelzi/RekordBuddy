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

#include <TraktorCollection/Crates/Playlist.hpp>
#include <TraktorCollection/Crates/Folder.hpp>
#include <TraktorCollection/Tracks/Track.hpp>
#include <TraktorCollection/Collection.hpp>

using namespace NxA;
using namespace Traktor;

// -- Constructors & Destructors

MutablePlaylist::MutablePlaylist(MutableXMLNode withPlaylist,
                                 Pointer<MutableCollection> inCollection,
                                 NotNull<MutableFolder*> withParentFolder,
                                 const MutablePlaylist::Protected&) : p_collection{ inCollection },
                                                                      p_lastModificationTime{ (*p_collection).lastPlaylistsModificationTime() },
                                                                      p_traktorPlaylist{ std::move(withPlaylist) },
                                                                      p_parentFolder{ withParentFolder } { }

// -- Instance Methods

Optional<NotNull<const Common::Track*>> MutablePlaylist::p_maybeTrackForNode(const MutableXMLNode& xmlNode) const
{
    auto maybePlaylistKey = xmlNode.maybeStringValueForAttributeNamed("KEY");
    NXA_ASSERT_TRUE(maybePlaylistKey.isValid());

    return this->p_collection.asReference().maybeExistingTrackWithPlaylistPath(*maybePlaylistKey);
}

Optional<NotNull<Common::MutableTrack*>> MutablePlaylist::p_maybeTrackForNode(const MutableXMLNode& xmlNode)
{
    auto maybePlaylistKey = xmlNode.maybeStringValueForAttributeNamed("KEY");
    NXA_ASSERT_TRUE(maybePlaylistKey.isValid());

    return this->p_collection.asReference().maybeExistingTrackWithPlaylistPath(*maybePlaylistKey);
}

MutableArray<MutableXMLNode>& MutablePlaylist::p_ensureTracksAreLoaded() const
{
    if (!this->p_maybeSubNodes.isValid()) {
        this->p_maybeSubNodes = MutableArray<MutableXMLNode>{ };

        auto playlistNode = this->p_playlistNode();
        auto maybePlaylistType = playlistNode.maybeStringValueForAttributeNamed("TYPE");
        if (maybePlaylistType.isValid() && (maybePlaylistType == "LIST"_String)) {
            count index = 0;
            boolean playlistWasCorrected = false;

            for (auto&& entryNode : playlistNode.subNodesNamed("ENTRY")) {
                auto maybePrimaryKeyNode = entryNode.maybeFirstSubNodeNamed("PRIMARYKEY");
                if (!maybePrimaryKeyNode.isValid()) {
                    continue;
                }

                auto& primaryKeyNode = *maybePrimaryKeyNode;
                auto maybeEntryType = primaryKeyNode.maybeStringValueForAttributeNamed("TYPE");
                if (!maybeEntryType.isValid() || ((*maybeEntryType != "TRACK"_String) && (*maybeEntryType != "STEM"_String))) {
                    continue;
                }

                if (this->p_maybeTrackForNode(primaryKeyNode).isValid()) {
                    this->p_maybeSubNodes->append(primaryKeyNode);

                    ++index;
                }
                else {
                    playlistNode.deleteSubNodeNamedAtIndex("ENTRY", index);
                    playlistWasCorrected = true;
                }
            }

            if (playlistWasCorrected) {
                auto mutatedThis = const_cast<MutablePlaylist*>(this);
                mutatedThis->p_markAsModifiedNow();
            }
        }
    }

    return *this->p_maybeSubNodes;
}

Optional<count> MutablePlaylist::p_addExistingTrackAtIndex(NotNull<Common::MutableTrack*> track, count toIndex)
{
    auto maybeTraktorTrack = track.maybeAs<Traktor::MutableTrack*>();
    NXA_ASSERT_TRUE(maybeTraktorTrack.isValid());
    NXA_ASSERT_TRUE(this->collection() == track->collection());

    // -- We have to ensure the subnodes are loaded before we add the new subnode, otherwise we would end up adding it twice.
    auto& subNodes = this->p_ensureTracksAreLoaded();

    auto playlistNode = this->p_playlistNode();
    playlistNode.setCountValueForAttributeNamed(playlistNode.maybeCountValueForAttributeNamed("ENTRIES").valueOr(0) + 1, "ENTRIES");
    auto newEntryNode = playlistNode.appendSubNodeNamed("ENTRY");
    auto newKeyNode = newEntryNode.appendSubNodeNamed("PRIMARYKEY");
    newKeyNode.setStringValueForAttributeNamed("TRACK"_String, "TYPE");
    newKeyNode.setStringValueForAttributeNamed((*maybeTraktorTrack)->trackFilePathAsUsedInTraktorPlaylistEntries(), "KEY");
    subNodes.append(newKeyNode);

    auto currentIndex = subNodes.length() - 1;
    if (toIndex == currentIndex) {
        this->p_markAsModifiedNow();
        return toIndex;
    }

    count resultingIndex = currentIndex < toIndex ? (toIndex - 1) : toIndex;
    this->moveTrackAtIndexTo(currentIndex, toIndex);

    return resultingIndex;
}

void MutablePlaylist::p_markAsModifiedNow()
{
    this->p_lastModificationTime = Time::currentTime();
    this->p_collection.asReference().markPlaylistsAsModifiedNow();
}

NotNull<Common::MutableCollection*> MutablePlaylist::collection()
{
    return this->p_collection.asNotNull();
}

NotNull<const Common::Collection*> MutablePlaylist::collection() const
{
    return this->p_collection.asNotNull();
}

Optional<NotNull<Common::MutableFolder*>> MutablePlaylist::maybeParentFolder()
{
    return this->p_parentFolder.as<Common::MutableFolder*>();
}

Optional<NotNull<const Common::Folder*>> MutablePlaylist::maybeParentFolder() const
{
    return this->p_parentFolder.as<const Common::Folder*>();
}

void MutablePlaylist::setName(const String& name)
{
    this->p_traktorPlaylist.setStringValueForAttributeNamed(this->p_parentFolder->nextAvailableNameForPlaylistNamed(name), "NAME");

    this->p_markAsModifiedNow();
}

NotNull<Common::MutableTrack*> MutablePlaylist::trackAtIndex(count index)
{
    auto maybeTrack = this->p_maybeTrackForNode(this->p_ensureTracksAreLoaded()[index]);
    NXA_ASSERT_TRUE(maybeTrack.isValid());

    return *maybeTrack;
}

NotNull<const Common::Track*> MutablePlaylist::trackAtIndex(count index) const
{
    auto maybeTrack = this->p_maybeTrackForNode(this->p_ensureTracksAreLoaded()[index]);
    NXA_ASSERT_TRUE(maybeTrack.isValid());

    return *maybeTrack;
}

void MutablePlaylist::moveTracksAtIndicesToIndex(Array<count> indices, count toIndex)
{
    this->p_ensureTracksAreLoaded().moveObjectsAtIndicesTo(indices, toIndex);
    this->p_playlistNode().moveNodesNamedAtIndicesToIndex("ENTRY", indices, toIndex);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::moveTrackAtIndexTo(count index, count toIndex)
{
    this->p_ensureTracksAreLoaded().moveObjectAtIndexTo(index, toIndex);
    this->p_playlistNode().moveNodeNamedAtIndexTo("ENTRY", index, toIndex);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::removeTrackAtIndex(count index)
{
    this->p_ensureTracksAreLoaded().removeObjectAtIndex(index);
    this->p_playlistNode().deleteSubNodeNamedAtIndex("ENTRY", index);

    this->p_markAsModifiedNow();
}

void MutablePlaylist::removeAllTracks()
{
    this->p_maybeSubNodes = nothing;
    this->p_playlistNode().deleteSubNodesNamed("ENTRY");

    this->p_markAsModifiedNow();
}
