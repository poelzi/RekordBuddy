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

#include <TraktorCollection/Collection.hpp>
#include "TraktorCollection/Tracks/Track.hpp"
#include "TraktorCollection/Crates/Folder.hpp"
#include "TraktorCollection/Crates/AllTracksPlaylist.hpp"

#include <RekordBuddyCollection/UserPreferences.hpp>

using namespace NxA;
using namespace NxA::Traktor;

// -- Instance Methods

Optional<MutableXMLNode> MutableCollection::p_maybeNMLNode() const
{
    auto& maybeXMLDocument = const_cast<MutableCollection*>(this)->p_maybeXMLDocument;
    NXA_ASSERT_TRUE(maybeXMLDocument.isValid());

    auto maybeTopNode = (*maybeXMLDocument)->maybeFirstNode();
    if (!maybeTopNode.isValid() || !(*maybeTopNode).name().isEqualToWhichHasSize("NML", 3)) {
        return nothing;
    }

    return maybeTopNode;
}

Optional<MutableXMLNode> MutableCollection::p_maybePlaylistsNode() const
{
    auto maybeNMLNode = this->p_maybeNMLNode();
    if (!maybeNMLNode.isValid()) {
        return nothing;
    }

    return maybeNMLNode->maybeFirstSubNodeNamed("PLAYLISTS");
}

boolean MutableCollection::p_hasAValidAndSupportedTraktorNML() const
{
    auto maybeNMLNode = this->p_maybeNMLNode();
    if (!maybeNMLNode.isValid()) {
        return false;
    }

    // -- Get the version number.
    auto maybeVersionString = maybeNMLNode->maybeStringValueForAttributeNamed("VERSION");
    if (!maybeVersionString.isValid())  {
        return false;
    }

    // -- Make sure the HEAD node is there.
    auto maybeHeadNode = maybeNMLNode->maybeFirstSubNodeNamed("HEAD");
    if (!maybeHeadNode.isValid()) {
        return false;
    }

    // -- Make sure the HEAD info is correct.
    auto maybeCompany = maybeHeadNode->maybeStringValueForAttributeNamed("COMPANY");
    if (!maybeCompany.isValid() || (*maybeCompany != "www.native-instruments.com"_String))  {
        return false;
    }
    auto maybeProgram = maybeHeadNode->maybeStringValueForAttributeNamed("PROGRAM");
    if (!maybeProgram.isValid() || (*maybeProgram != "Traktor"_String))  {
        return false;
    }

    return true;
}

NotNull<Traktor::MutableFolder*> MutableCollection::p_rootFolder() const
{
    if (!this->p_maybeRootFolder.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        auto maybePlaylistsNode = this->p_maybePlaylistsNode();
        NXA_ASSERT_TRUE(maybePlaylistsNode.isValid());

        auto maybeRootFolderNode = maybePlaylistsNode->maybeFirstSubNodeNamed("NODE");
        NXA_ASSERT_TRUE(maybeRootFolderNode.isValid());

        auto& rootFolderNode = *maybeRootFolderNode;

        auto maybeRootFolderNodeType = rootFolderNode.maybeStringValueForAttributeNamed("TYPE");
        NXA_ASSERT_TRUE(maybeRootFolderNodeType.isValid());
        NXA_ASSERT_EQ(*maybeRootFolderNodeType, "FOLDER"_String);

        auto maybeRootFolderNodeName = rootFolderNode.maybeStringValueForAttributeNamed("NAME");
        NXA_ASSERT_TRUE(maybeRootFolderNodeName.isValid());
        NXA_ASSERT_EQ(*maybeRootFolderNodeName, "$ROOT"_String);

        this->p_maybeRootFolder = Unique<MutableFolder>::with(rootFolderNode,
                                                              const_cast<MutableCollection*>(this),
                                                              nothing,
                                                              MutableFolder::p_isProtected);
    }

    return this->p_maybeRootFolder->asRawPointer();
}

NotNull<Traktor::MutableAllTracksPlaylist*> MutableCollection::p_tracks() const
{
    if (!this->p_maybeAllTracksPlaylist.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeAllTracksPlaylist = Unique<MutableAllTracksPlaylist>::with(const_cast<MutableCollection*>(this), MutableAllTracksPlaylist::p_isProtected);
    }

    return this->p_maybeAllTracksPlaylist->asRawPointer();
}

Optional<Common::Collection::Error> MutableCollection::open()
{
    if (this->isOpened()) {
        return nothing;
    }

    auto& collectionFilePath = this->p_nmlFilePath;
    auto maybeFileContent = File::maybeContentOfFileAtAsString(collectionFilePath);
    if (!maybeFileContent.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::CannotOpen;
    }

    this->p_maybeXMLDocument = XMLDocument::maybeWithString(maybeFileContent->asNormalizedString());
    if (!this->p_maybeXMLDocument.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    if (!this->p_hasAValidAndSupportedTraktorNML()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    auto maybeCollectionNode = this->p_maybeNMLNode()->maybeFirstSubNodeNamed("COLLECTION");
    if (!maybeCollectionNode.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    this->p_maybeAllTracksPlaylistNode = *maybeCollectionNode;

    this->p_lastModificationTime = File::modificationTimeForFile(collectionFilePath);
    this->p_allTracksLastModificationTime = this->p_lastModificationTime;
    this->p_playlistsLastModificationTime = this->p_lastModificationTime;
    this->p_lastSavingTime = this->p_lastModificationTime;

    this->notifyUserPreferencesHaveChanged();

    return this->p_lastOpenResult = nothing;
}

void MutableCollection::notifyUserPreferencesHaveChanged()
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    this->p_artistsSeparator = *userPreferences->maybeStringForKey(String{ NXA_ARTISTS_SEPARATOR_PREFERENCES_KEY });
    this->p_genresSeparator = *userPreferences->maybeStringForKey(String{ NXA_GENRES_SEPARATOR_PREFERENCES_KEY });
    this->p_musicalKeysSeparator = *userPreferences->maybeStringForKey(String{ NXA_MUSICAL_KEYS_SEPARATOR_PREFERENCES_KEY });
    this->p_useComment2FieldAsGrouping = *userPreferences->maybeBooleanForKey(String{ NXA_IMPORT_EXPORT_TRAKTOR_COMMENTS2_FIELD_AS_GROUPING_PREFERENCES_KEY });
    this->p_mergeGridMarkersAndHotCues = *userPreferences->maybeBooleanForKey(String{ NXA_MERGE_GRID_MARKERS_AND_HOT_CUES_FOR_TRAKTOR_PREFERENCES_KEY });
    this->p_readTraktorKeyInsteadOfKeyText = *userPreferences->maybeBooleanForKey(String{ NXA_READ_TRAKTOR_KEY_INSTEAD_OF_KEY_TEXT_PREFERENCES_KEY });
}

NotNull<const Common::Folder*> MutableCollection::rootFolder() const
{
    return this->p_rootFolder();
}

NotNull<Common::MutableFolder*> MutableCollection::rootFolder()
{
    return this->p_rootFolder();
}

NotNull<const Common::Playlist*> MutableCollection::tracks() const
{
    return this->p_tracks();
}

NotNull<Common::MutablePlaylist*> MutableCollection::tracks()
{
    return this->p_tracks();
}

NotNull<Common::MutableTrack*> MutableCollection::trackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    NXA_ASSERT_TRUE(this->isOpened());

    auto maybeExistingTrack = this->maybeExistingTrackWithAbsoluteFilePath(absoluteFilePath);
    if (maybeExistingTrack.isValid()) {
        return *maybeExistingTrack;
    }

    auto newNode = this->p_maybeAllTracksPlaylistNode->appendSubNodeNamed("ENTRY");
    MutableTrack::p_setModificationTimeOnNode(Time::currentTime(), newNode);

    auto trackVolume = Volume{ absoluteFilePath };
    auto relativeFilePath = *absoluteFilePath.maybeRelativeToVolume(trackVolume);

    auto pathComponents = relativeFilePath.componentsOfPath();
    MutableArray<String> pathComponentsAsString;
    for (auto&& component : pathComponents) {
        pathComponentsAsString.append(component.asEncodedString().stringByReplacingOccurencesOfWith(":", "//"));
    }

    auto newLocationNode = newNode.appendSubNodeNamed("LOCATION");
    newLocationNode.setStringValueForAttributeNamed(pathComponentsAsString.lastObject(), "FILE");

    pathComponentsAsString.removeLastObject();
    newLocationNode.setStringValueForAttributeNamed(pathComponentsAsString.length() ? String::stringWithFormat("/:%s/:", String::stringByJoiningArrayWithString(pathComponentsAsString, "/:"_String).asUTF8())
                                                                                    : "/:",
                                                    "DIR");

    auto volumeName = trackVolume.name();
    newLocationNode.setStringValueForAttributeNamed(volumeName, "VOLUME");
    newLocationNode.setStringValueForAttributeNamed(volumeName, "VOLUMEID");

    auto& tracks = *this->p_maybeTraktorTracks;
    tracks.append(Shared<MutableTrack>::with(newNode, this, MutableTrack::p_isProtected));

    auto lastTrackAdded = tracks.lastObject().asRawPointer();
    this->p_traktorAbsoluteFilePathPerPlaylistPath.setValueForKey(absoluteFilePath, lastTrackAdded->trackFilePathAsUsedInTraktorPlaylistEntries());
    NXA_ASSERT_TRUE(this->p_traktorTracksPerAbsoluteFilePath.setValueForKeyCausedAnInsertion(lastTrackAdded, absoluteFilePath));

    this->markAllTracksAsModifiedNow();

    return lastTrackAdded;
}

Optional<NotNull<const Common::Track*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath) const
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return this->p_traktorTracksPerAbsoluteFilePath.maybeValueForKey(absoluteFilePath).maybe([](auto&& track) {
        return NotNull<const Common::Track*>{ track };
    });
}

Optional<NotNull<Common::MutableTrack*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return this->p_traktorTracksPerAbsoluteFilePath.maybeValueForKey(absoluteFilePath).maybe([](auto&& track) {
        return NotNull<Common::MutableTrack*>{ track };
    });
}

Optional<NotNull<const Common::Track*>> MutableCollection::maybeExistingTrackWithPlaylistPath(const String& playlistPath) const
{
    this->p_ensureTracksAreParsed();

    auto maybeAbsoluteFilePath = this->p_traktorAbsoluteFilePathPerPlaylistPath.maybeValueForKey(playlistPath);
    if (!maybeAbsoluteFilePath.isValid()) {
        return nothing;
    }

    return this->maybeExistingTrackWithAbsoluteFilePath(*maybeAbsoluteFilePath);
}

Optional<NotNull<Common::MutableTrack*>> MutableCollection::maybeExistingTrackWithPlaylistPath(const String& playlistPath)
{
    this->p_ensureTracksAreParsed();

    auto maybeAbsoluteFilePath = this->p_traktorAbsoluteFilePathPerPlaylistPath.maybeValueForKey(playlistPath);
    if (!maybeAbsoluteFilePath.isValid()) {
        return nothing;
    }

    return this->maybeExistingTrackWithAbsoluteFilePath(*maybeAbsoluteFilePath);
}
