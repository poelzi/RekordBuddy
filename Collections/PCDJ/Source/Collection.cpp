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
#include <PCDJCollection/Tracks/Track.hpp>
#include <PCDJCollection/Crates/Folder.hpp>
#include <PCDJCollection/Crates/AllTracksPlaylist.hpp>
#include <PCDJCollection/PCDJ.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/CrashLog.hpp>

using namespace NxA;
using namespace NxA::PCDJ;

// -- Class Methods

Optional<MutableXMLNode> MutableCollection::p_maybeDatabaseNodeForDocument(XMLDocument& document)
{
    auto maybeTopNode = document.maybeFirstNode();
    if (!maybeTopNode.isValid() || ((*maybeTopNode).name() != "database"_String)) {
        return nothing;
    }

    return maybeTopNode;
}

Optional<String> MutableCollection::maybeErrorIfNotValidPCDJXML(XMLDocument& document)
{
    auto maybeDatabaseNode = MutableCollection::p_maybeDatabaseNodeForDocument(document);
    if (!maybeDatabaseNode.isValid() || (maybeDatabaseNode->maybeIntegerValueForAttributeNamed("version").valueOr(0) != 1)) {
        return { "Invalid PCDJ XML"_String };
    }

    return nothing;
}

// -- Constructors & Destructors

MutableCollection::MutableCollection(const FilePath& xmlFilePath) : p_xmlFilePath(xmlFilePath)
{
    this->reset();
}

// -- Instance Methods

Optional<MutableXMLNode> MutableCollection::p_maybeDatabaseNode() const
{
    auto& maybeXMLDocument = const_cast<MutableCollection*>(this)->p_maybeXMLDocument;
    if (!maybeXMLDocument.isValid()) {
        return nothing;
    }

    return MutableCollection::p_maybeDatabaseNodeForDocument(**maybeXMLDocument);
}

Optional<MutableXMLNode> MutableCollection::p_maybeTracksNode() const
{
    auto maybeDatabaseNode = this->p_maybeDatabaseNode();
    if (!maybeDatabaseNode.isValid()) {
        return nothing;
    }

    auto tracksNodes = maybeDatabaseNode->subNodesNamed("tracks");
    if (tracksNodes.length() != 1) {
        return nothing;
    }

    return tracksNodes.firstObject();
}

boolean MutableCollection::p_hasAValidPCDJXML() const
{
    auto& maybeXMLDocument = const_cast<MutableCollection*>(this)->p_maybeXMLDocument;
    NXA_ASSERT_TRUE(maybeXMLDocument.isValid());

    this->p_maybeOpenErrorDescription = MutableCollection::maybeErrorIfNotValidPCDJXML(**maybeXMLDocument);
    return !this->p_maybeOpenErrorDescription.isValid();
}

NotNull<PCDJ::MutableFolder*> MutableCollection::p_rootFolder() const
{
    if (!this->p_maybeRootFolder.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        auto maybeDatabaseNode = this->p_maybeDatabaseNode();
        NXA_ASSERT_TRUE(maybeDatabaseNode.isValid());

        auto listsNode = maybeDatabaseNode->subNodesNamed("lists");
        NXA_ASSERT_TRUE(listsNode.length() == 1);

        this->p_maybeRootFolder = Unique<MutableFolder>::with(listsNode.firstObject(),
                                                              const_cast<MutableCollection*>(this),
                                                              nothing,
                                                              MutableFolder::p_isProtected);
    }

    return this->p_maybeRootFolder->asRawPointer();
}

NotNull<PCDJ::MutableAllTracksPlaylist*> MutableCollection::p_tracks() const
{
    if (!this->p_maybeAllTracksPlaylist.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeAllTracksPlaylist = Unique<MutableAllTracksPlaylist>::with(const_cast<MutableCollection*>(this), MutableAllTracksPlaylist::p_isProtected);
    }

    return this->p_maybeAllTracksPlaylist->asRawPointer();
}

Optional<Common::Collection::Error> MutableCollection::open()
{
    auto maybeFileContent = File::maybeContentOfFileAtAsString(this->p_xmlFilePath);
    if (!maybeFileContent.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::CannotOpen;
    }

    this->p_maybeXMLDocument = XMLDocument::maybeWithString(maybeFileContent->asNormalizedString());
    if (!this->p_maybeXMLDocument.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    if (!this->p_hasAValidPCDJXML()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    this->p_lastModificationTime = File::modificationTimeForFile(this->p_xmlFilePath);
    this->p_playlistsLastModificationTime = this->p_lastModificationTime;
    this->p_allTracksLastModificationTime = this->p_lastModificationTime;
    this->p_lastSavingTime = this->p_lastModificationTime;

    this->p_maybeAllTracksPlaylistNode = this->p_maybeTracksNode();

    this->notifyUserPreferencesHaveChanged();

    return this->p_lastOpenResult = nothing;
}

void MutableCollection::notifyUserPreferencesHaveChanged()
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    this->p_artistsSeparator = *userPreferences->maybeStringForKey(String{ NXA_ARTISTS_SEPARATOR_PREFERENCES_KEY });
    this->p_genresSeparator = *userPreferences->maybeStringForKey(String{ NXA_GENRES_SEPARATOR_PREFERENCES_KEY });
    this->p_musicalKeysSeparator = *userPreferences->maybeStringForKey(String{ NXA_MUSICAL_KEYS_SEPARATOR_PREFERENCES_KEY });
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

    auto newNode = this->p_maybeAllTracksPlaylistNode->appendSubNodeNamed("track");
    auto newTrackID = ++this->p_maximumTrackIDFound;

    newNode.setCountValueForAttributeNamed(newTrackID, "id");
    newNode.setStringValueForAttributeNamed(absoluteFilePath.asEncodedString().asStringByAddingPercentEncoding(), "fnam");

    auto& tracks = *this->p_maybePCDJTracks;
    tracks.append(Shared<MutableTrack>::with(newNode, this, MutableTrack::p_isProtected));

    auto lastTrackAdded = tracks.lastObject().asRawPointer();
    NXA_ASSERT_TRUE(this->p_pcdjTracksPerAbsoluteFilePath.setValueForKeyCausedAnInsertion(lastTrackAdded, absoluteFilePath));

    lastTrackAdded->setTrackID(newTrackID);
    this->p_pcdjTracksPerTrackID.setValueForKey(lastTrackAdded, newTrackID);

    this->markAllTracksAsModifiedNow();

    return lastTrackAdded;
}

Optional<NotNull<const Common::Track*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath) const
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return this->p_pcdjTracksPerAbsoluteFilePath.maybeValueForKey(absoluteFilePath).maybe([](auto&& track) {
        return NotNull<const Common::Track*>{ track };
    });
}

Optional<NotNull<Common::MutableTrack*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return this->p_pcdjTracksPerAbsoluteFilePath.maybeValueForKey(absoluteFilePath).maybe([](auto&& track) {
        return NotNull<Common::MutableTrack*>{ track };
    });
}

void MutableCollection::removeTrackAtIndex(count index)
{
    NXA_ASSERT_TRUE(this->isOpened());

    auto& tracks = this->p_ensureTracksAreParsed();
    auto& existingTrack = tracks[index];

    auto maybeTrackID = existingTrack->maybeTrackID();
    if (maybeTrackID.isValid()) {
        this->p_pcdjTracksPerTrackID.removeValueForKey(*maybeTrackID);
    }

    this->p_pcdjTracksPerAbsoluteFilePath.removeValueForKey(existingTrack->absoluteFilePath());

    this->p_maybeAllTracksPlaylistNode->deleteSubNodeNamedAtIndex("track", index);

    tracks.removeObjectAtIndex(index);

    this->markAsModifiedNow();
}

void MutableCollection::removeAllTracks()
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_maybeAllTracksPlaylistNode->deleteSubNodesNamed("track");
    this->p_maybePCDJTracks = MutableArray<Shared<MutableTrack>>{ };
    this->p_pcdjTracksPerTrackID.removeAll();
    this->p_pcdjTracksPerAbsoluteFilePath.removeAll();
    this->p_maximumTrackIDFound = 0;

    this->markAsModifiedNow();
}
