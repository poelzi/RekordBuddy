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

#include <AppleMusicCollection/Collection.hpp>
#include <AppleMusicCollection/Tracks/Track.hpp>
#include <AppleMusicCollection/Crates/Folder.hpp>
#include <AppleMusicCollection/Crates/AllTracksPlaylist.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

using namespace NxA;
using namespace NxA::AppleMusic;

// -- Class Methods

Optional<XMLNode> Collection::p_maybePListNodeForDocument(NotNull<const XMLDocument*> document)
{
    auto maybeTopNode = document->maybeFirstNode();
    if (!maybeTopNode.isValid() || ((*maybeTopNode).name() != "plist"_String)) {
        return nothing;
    }

    auto maybeVersion = maybeTopNode->maybeStringValueForAttributeNamed("version");
    if (!maybeVersion.isValid() || (*maybeVersion != "1.0"_String)) {
        return nothing;
    }

    return maybeTopNode;
}

Optional<XMLNode> Collection::p_maybeTopDictNodeForDocument(NotNull<const XMLDocument*> document)
{
    auto maybePListNode = Collection::p_maybePListNodeForDocument(document);
    if (!maybePListNode.isValid()) {
        return nothing;
    }

    auto maybeTopDictNode = maybePListNode->maybeFirstSubNodeNamed("dict");
    if (!maybeTopDictNode.isValid()) {
        return nothing;
    }

    return maybeTopDictNode;
}

boolean Collection::p_isAValidAndSupportedITunesXML(NotNull<const XMLDocument*> document,
                                                    Optional<XMLNode>& maybeAllTracksPlaylistNode,
                                                    Optional<XMLNode>& maybePlaylistsNode)
{
    auto maybeTopDictNode = Collection::p_maybeTopDictNodeForDocument(document);
    if (!maybeTopDictNode.isValid()) {
        return false;
    }

    auto maybeNode = maybeTopDictNode->maybeFirstSubNode();
    while (maybeNode.isValid()) {
        auto name = maybeNode->name();
        if (name == "key"_String) {
            auto maybeKey = maybeNode->maybeValue();
            if (!maybeKey.isValid()) {
                return false;
            }

            auto& key = *maybeKey;

            maybeNode = maybeNode->maybeSiblingNode();
            if (!maybeNode.isValid()) {
                return false;
            }

            if ((key == "Major Version"_String) && (maybeNode->name() == "integer"_String)){
                auto maybeValue = maybeNode->maybeValue();
                if (maybeValue.isValid()) {
                    if (maybeValue->integerValue() != 1) {
                        return false;
                    }
                }
            }
            else if ((key == "Minor Version"_String) && (maybeNode->name() == "integer"_String)) {
                auto maybeValue = maybeNode->maybeValue();
                if (maybeValue.isValid()) {
                    if (maybeValue->integerValue() != 1) {
                        return false;
                    }
                }
            }
            else if ((key == "Tracks"_String) && (maybeNode->name() == "dict"_String)) {
                if (maybeAllTracksPlaylistNode.isValid()) {
                    return false;
                }

                maybeAllTracksPlaylistNode = *maybeNode;
            }
            else if ((key == "Playlists"_String) && (maybeNode->name() == "array"_String)) {
                if (maybePlaylistsNode.isValid()) {
                    return false;
                }

                maybePlaylistsNode = *maybeNode;
            }
        }

        maybeNode = maybeNode->maybeSiblingNode();
    }

    return maybeAllTracksPlaylistNode.isValid() && maybePlaylistsNode.isValid();
}

boolean Collection::isAValidAndSupportedITunesXML(XMLDocument& document)
{
    Optional<XMLNode> maybeAllTracksPlaylistNode;
    Optional<XMLNode> maybePlaylistsNode;

    return Collection::p_isAValidAndSupportedITunesXML({ &document }, maybeAllTracksPlaylistNode, maybePlaylistsNode);
}

// -- Instance Methods

Optional<XMLNode> Collection::p_maybePListNode() const
{
    auto& maybeXMLDocument = this->p_maybeXMLDocument;
    NXA_ASSERT_TRUE(maybeXMLDocument.isValid());

    auto maybeTopNode = (*maybeXMLDocument)->maybeFirstNode();
    if (!maybeTopNode.isValid() || ((*maybeTopNode).name() != "plist"_String)) {
        return nothing;
    }

    auto maybeVersion = maybeTopNode->maybeStringValueForAttributeNamed("version");
    if (!maybeVersion.isValid() || (*maybeVersion != "1.0"_String)) {
        return nothing;
    }

    return maybeTopNode;
}

Optional<XMLNode> Collection::p_maybeTopDictNode() const
{
    auto maybePListNode = this->p_maybePListNode();
    if (!maybePListNode.isValid()) {
        return nothing;
    }

    auto maybeTopDictNode = maybePListNode->maybeFirstSubNodeNamed("dict");
    if (!maybeTopDictNode.isValid()) {
        return nothing;
    }

    return maybeTopDictNode;
}

NotNull<AppleMusic::Folder*> Collection::p_rootFolder() const
{
    if (!this->p_maybeRootFolder.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        auto maybeNode = this->p_maybePlaylistsNode->maybeFirstSubNode();
        while (maybeNode.isValid()) {
            auto name = maybeNode->name();
            if (name == "dict"_String) {
                Optional<String> maybeCurrentPersistentID;
                Optional<String> maybeParentPersistentID;
                Optional<String> maybePlaylistName;

                auto maybeSubNode = maybeNode->maybeFirstSubNode();
                while (maybeSubNode.isValid()) {
                    if (maybeSubNode->name() == "key"_String) {
                        auto maybeKey = maybeSubNode->maybeValue();
                        if (maybeKey.isValid()) {
                            auto& key = *maybeKey;

                            maybeSubNode = maybeSubNode->maybeSiblingNode();
                            if (maybeSubNode.isValid()) {
                                if ((key == "Visible"_String) && (maybeSubNode->name() == "false"_String)) {
                                    maybePlaylistName = nothing;
                                    break;
                                }
                                else if ((key == "Name"_String) && (maybeSubNode->name() == "string"_String)) {
                                    maybePlaylistName = maybeSubNode->maybeValue();
                                }
                                else if ((key == "Playlist Persistent ID"_String) && (maybeSubNode->name() == "string"_String)) {
                                    auto maybeID = maybeSubNode->maybeValue();
                                    if (maybeID.isValid()) {
                                        maybeCurrentPersistentID = maybeID;
                                    }
                                }
                                else if ((key == "Parent Persistent ID"_String) && (maybeSubNode->name() == "string"_String)) {
                                    auto maybeParentID = maybeSubNode->maybeValue();
                                    if (maybeParentID.isValid()) {
                                        maybeParentPersistentID = *maybeParentID;
                                    }
                                }
                                else if (key == "Distinguished Kind"_String) {
                                    // -- These are internal AppleMusic playlists so we skip them.
                                    maybePlaylistName = nothing;
                                    break;
                                }
                            }
                        }
                    }

                    maybeSubNode = maybeSubNode->maybeSiblingNode();
                }

                if (maybePlaylistName.isValid() && maybeCurrentPersistentID.isValid()) {
                    this->p_xmlNodesPerPersistentID.setValueForKey(*maybeNode, *maybeCurrentPersistentID);
                    this->p_playlistNamePerPersistentID.setValueForKey(*maybePlaylistName,
                                                                       *maybeCurrentPersistentID);

                    if (maybeParentPersistentID.isValid()) {
                        auto& childPersistentIDs = this->p_persistentIDsPerParentPersistentID.valueForKeyOrSetWith(*maybeParentPersistentID,
                                                                                                                   []() {
                                                                                                                       return MutableArray<String>{ };
                                                                                                                   });
                        childPersistentIDs.append(*maybeCurrentPersistentID);
                    }
                    else {
                        // -- This playlist doesn't have a parent so it's in the root.
                        this->p_rootPersistentIDs.append(*maybeCurrentPersistentID);
                    }
                }
            }

            maybeNode = maybeNode->maybeSiblingNode();
        }

        this->p_maybeRootFolder = Unique<Folder>::with("<Root Folder>"_String,
                                                       this->p_rootPersistentIDs,
                                                       const_cast<Collection*>(this),
                                                       nothing,
                                                       Folder::p_isProtected);
    }

    return this->p_maybeRootFolder->asRawPointer();
}

NotNull<AppleMusic::AllTracksPlaylist*> Collection::p_tracks() const
{
    if (!this->p_maybeAllTracksPlaylist.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeAllTracksPlaylist = Unique<AllTracksPlaylist>::with(const_cast<Collection*>(this), AllTracksPlaylist::p_isProtected);
    }

    return this->p_maybeAllTracksPlaylist->asRawPointer();
}

const MutableArray<Shared<Track>>& Collection::p_ensureTracksAreParsed() const
{
    if (!this->p_maybeITunesTracks.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeITunesTracks = MutableArray<Shared<Track>>{ };

        auto& tracks = *this->p_maybeITunesTracks;
        auto mutatedThis = const_cast<Collection*>(this);

        auto maybeNode = this->p_maybeAllTracksPlaylistNode->maybeFirstSubNode();
        while (maybeNode.isValid()) {
            if (maybeNode->name() == "key"_String) {
                auto maybeNextNode = maybeNode->maybeSiblingNode();
                if (maybeNextNode.isValid() && (maybeNextNode->name() == "dict"_String)) {
                    auto maybeKey = maybeNode->maybeValue();
                    if (maybeKey.isValid()) {
                        boolean isAFileType = false;

                        auto maybeTrackNode = maybeNextNode->maybeFirstSubNode();
                        while (maybeTrackNode.isValid()) {
                            if (maybeTrackNode->name() == "key"_String) {
                                auto maybeTrackNodeValue = maybeTrackNode->maybeValue();

                                maybeTrackNode = maybeTrackNode->maybeSiblingNode();

                                if (maybeTrackNodeValue.isValid() && *maybeTrackNodeValue == "Track Type") {
                                    if (maybeTrackNode->name() == "string"_String) {
                                        maybeTrackNodeValue = maybeTrackNode->maybeValue();
                                        if (maybeTrackNodeValue.isValid() && *maybeTrackNodeValue == "File") {
                                            isAFileType = true;
                                            break;
                                        }
                                    }
                                }
                            }

                            maybeTrackNode = maybeTrackNode->maybeSiblingNode();
                        }

                        if (isAFileType) {
                            auto maybeNewTrack = Track::maybeTrackWithNodeInCollection(*maybeNextNode, mutatedThis, Track::p_isProtected);
                            if (maybeNewTrack.isValid()) {
                                tracks.append(*maybeNewTrack);

                                this->p_appleMusicTracksPerTrackID.setValueForKey(maybeNewTrack->asRawPointer(), maybeKey->integerValue());
                            }
                        }
                    }
                }
            }

            maybeNode = maybeNode->maybeSiblingNode();
        }
    }

    return *this->p_maybeITunesTracks;
}

Optional<Common::Collection::Error> Collection::open()
{
    if (this->isOpened()) {
        return nothing;
    }

    auto collectionFilePath = this->p_xmlFilePath;
    auto maybeFileContent = File::maybeContentOfFileAtAsString(collectionFilePath);
    if (!maybeFileContent.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::CannotOpen;
    }

    this->p_maybeXMLDocument = XMLDocument::maybeWithString(maybeFileContent->asNormalizedString());
    if (!this->p_maybeXMLDocument.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    if (!Collection::p_isAValidAndSupportedITunesXML({ (*this->p_maybeXMLDocument).asRawPointer() },
                                                     this->p_maybeAllTracksPlaylistNode,
                                                     this->p_maybePlaylistsNode)) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    this->p_lastModificationTime = File::modificationTimeForFile(collectionFilePath);

    this->notifyUserPreferencesHaveChanged();

    return this->p_lastOpenResult = nothing;
}

void Collection::notifyUserPreferencesHaveChanged()
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    this->p_artistsSeparator = *userPreferences->maybeStringForKey(String{ NXA_ARTISTS_SEPARATOR_PREFERENCES_KEY });
    this->p_genresSeparator = *userPreferences->maybeStringForKey(String{ NXA_GENRES_SEPARATOR_PREFERENCES_KEY });
    this->p_musicalKeysSeparator = *userPreferences->maybeStringForKey(String{ NXA_MUSICAL_KEYS_SEPARATOR_PREFERENCES_KEY });
}

NotNull<const Common::Folder*> Collection::rootFolder() const
{
    return this->p_rootFolder();
}

NotNull<const Common::Playlist*> Collection::tracks() const
{
    return this->p_tracks();
}

Optional<NotNull<const Common::Track*>> Collection::maybeExistingTrackWithAbsoluteFilePath(const FilePath&) const
{
    // -- This should not be necessary for AppleMusic collections.
    NXA_ALOG("Unimplemented.");
}

Optional<NotNull<const Common::Track*>> Collection::maybeExistingTrackWithRelativeFilePath(const FilePath&) const
{
    // -- This should not be necessary for AppleMusic collections.
    NXA_ALOG("Unimplemented.");
}
