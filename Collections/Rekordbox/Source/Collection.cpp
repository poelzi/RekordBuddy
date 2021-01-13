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

#include <RekordboxCollection/Collection.hpp>
#include <RekordboxCollection/Tracks/Track.hpp>
#include <RekordboxCollection/Crates/Folder.hpp>
#include <RekordboxCollection/Crates/AllTracksPlaylist.hpp>
#include <RekordboxCollection/Rekordbox.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/CrashLog.hpp>

using namespace NxA;
using namespace NxA::Rekordbox;

// -- Class Methods

Optional<MutableXMLNode> MutableCollection::p_maybeDJPlaylistsNodeForDocument(XMLDocument& document)
{
    auto maybeTopNode = document.maybeFirstNode();
    if (!maybeTopNode.isValid() || ((*maybeTopNode).name() != "DJ_PLAYLISTS"_String)) {
        return nothing;
    }

    return maybeTopNode;
}

Optional<MutableXMLNode> MutableCollection::p_maybeProductNodeForDocument(XMLDocument& document)
{
    auto maybeDjPlaylistsNode = MutableCollection::p_maybeDJPlaylistsNodeForDocument(document);
    if (!maybeDjPlaylistsNode.isValid()) {
        return nothing;
    }

    auto maybeProductNode = maybeDjPlaylistsNode->maybeFirstSubNodeNamed("PRODUCT");
    if (!maybeProductNode.isValid()) {
        return nothing;
    }

    return *maybeProductNode;
}

Optional<String> MutableCollection::maybeErrorIfNotValidRekordboxXML(XMLDocument& document)
{
    auto maybeProductNode = MutableCollection::p_maybeProductNodeForDocument(document);
    if (!maybeProductNode.isValid()) {
        return { "Invalid rekordbox XML"_String };
    }

    auto& productNode = *maybeProductNode;
    auto appName = productNode.maybeStringValueForAttributeNamed("Name").valueOr(String{ });
    auto companyName = productNode.maybeStringValueForAttributeNamed("Company").valueOr(String{ });
    if (appName == "rekordbox"_String) {
        if ((companyName != "Pioneer DJ"_String) && (companyName != "AlphaTheta"_String)) {
            return { String::stringWithFormat("XML file written by unknown company '%s'.", companyName.asUTF8()) };
        }
    }
    else if (appName == "Rekord Buddy"_String) {
        if ((companyName != "Next Audio Labs, LLC."_String) &&
            (companyName != "Next Audio Labs"_String) &&
            (companyName != "Didier Malenfant"_String) &&
            (companyName != "next.audio"_String)) {
            return { String::stringWithFormat("XML file written by unknown company '%s'.", companyName.asUTF8()) };
        }
    }
    else {
        return { String::stringWithFormat("XML file written by unknown program '%s'.", appName.asUTF8()) };
    }

    return nothing;
}

// -- Constructors & Destructors

MutableCollection::MutableCollection(const FilePath& xmlFilePath,
                                     boolean isAtAUserLocation) : p_xmlFilePath(xmlFilePath),
                                                                  p_isAtAUserLocation(isAtAUserLocation),
                                                                  p_tracksWillNeedRenumbering(false)
{
    this->reset();
}

// -- Instance Methods

Optional<Common::Collection::Error> MutableCollection::p_openWithFileAt(const FilePath& collectionFilePath)
{
    if (this->isOpened()) {
        return nothing;
    }

    auto maybeFileContent = File::maybeContentOfFileAtAsString(collectionFilePath);
    if (!maybeFileContent.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::CannotOpen;
    }

    this->p_maybeXMLDocument = XMLDocument::maybeWithString(maybeFileContent->asNormalizedString());
    if (!this->p_maybeXMLDocument.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    if (!this->p_hasAValidRekordboxXML()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    auto maybeDjPlaylistsNode = this->p_maybeDJPlaylistsNode();
    if (!maybeDjPlaylistsNode.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    auto maybeCollectionNode = maybeDjPlaylistsNode->maybeFirstSubNodeNamed("COLLECTION");
    if (!maybeCollectionNode.isValid()) {
        return this->p_lastOpenResult = Common::Collection::Error::Corrupted;
    }

    this->p_lastModificationTime = File::modificationTimeForFile(collectionFilePath);
    this->p_playlistsLastModificationTime = this->p_lastModificationTime;
    this->p_allTracksLastModificationTime = this->p_lastModificationTime;
    this->p_lastSavingTime = this->p_lastModificationTime;

    this->p_maybeAllTracksPlaylistNode = *maybeCollectionNode;

    this->notifyUserPreferencesHaveChanged();

    return this->p_lastOpenResult = nothing;
}

Optional<MutableXMLNode> MutableCollection::p_maybeDJPlaylistsNode() const
{
    auto& maybeXMLDocument = const_cast<MutableCollection*>(this)->p_maybeXMLDocument;
    NXA_ASSERT_TRUE(maybeXMLDocument.isValid());

    return MutableCollection::p_maybeDJPlaylistsNodeForDocument(**maybeXMLDocument);
}

Optional<MutableXMLNode> MutableCollection::p_maybeProductNode() const
{
    auto maybeDjPlaylistsNode = this->p_maybeDJPlaylistsNode();
    if (!maybeDjPlaylistsNode.isValid()) {
        return nothing;
    }

    auto maybeProductNode = maybeDjPlaylistsNode->maybeFirstSubNodeNamed("PRODUCT");
    if (!maybeProductNode.isValid()) {
        return nothing;
    }

    return *maybeProductNode;
}

boolean MutableCollection::p_hasAValidRekordboxXML() const
{
    auto& maybeXMLDocument = const_cast<MutableCollection*>(this)->p_maybeXMLDocument;
    NXA_ASSERT_TRUE(maybeXMLDocument.isValid());

    this->p_maybeOpenErrorDescription = MutableCollection::maybeErrorIfNotValidRekordboxXML(**maybeXMLDocument);
    return !this->p_maybeOpenErrorDescription.isValid();
}

NotNull<Rekordbox::MutableFolder*> MutableCollection::p_rootFolder() const
{
    if (!this->p_maybeRootFolder.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        auto maybeDjPlaylistsNode = this->p_maybeDJPlaylistsNode();
        NXA_ASSERT_TRUE(maybeDjPlaylistsNode.isValid());

        auto maybePlaylistsNode = maybeDjPlaylistsNode->maybeFirstSubNodeNamed("PLAYLISTS");
        NXA_ASSERT_TRUE(maybePlaylistsNode.isValid());

        auto maybeRootNode = maybePlaylistsNode->maybeFirstSubNodeNamed("NODE");
        NXA_ASSERT_TRUE(maybeRootNode.isValid());

        this->p_maybeRootFolder = Unique<MutableFolder>::with(*maybeRootNode, const_cast<MutableCollection*>(this), nothing, MutableFolder::p_isProtected);
    }

    return this->p_maybeRootFolder->asRawPointer();
}

NotNull<Rekordbox::MutableAllTracksPlaylist*> MutableCollection::p_tracks() const
{
    if (!this->p_maybeAllTracksPlaylist.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeAllTracksPlaylist = Unique<MutableAllTracksPlaylist>::with(const_cast<MutableCollection*>(this), MutableAllTracksPlaylist::p_isProtected);
    }

    return this->p_maybeAllTracksPlaylist->asRawPointer();
}

void MutableCollection::p_renumberTrackIDs()
{
    MutableMap<count, count> oldTrackIDToNewTrackID;
    count newTrackID = 1;

    for (auto&& track : this->p_ensureTracksAreParsed()) {
        auto maybeOldTrackID = track->maybeTrackID();
        if (!maybeOldTrackID.isValid()) {
            continue;
        }

        track->setTrackID(newTrackID);

        oldTrackIDToNewTrackID.setValueForKey(newTrackID, *maybeOldTrackID);

        ++newTrackID;
    }

    Map<count, count> convertionMap{ std::move(oldTrackIDToNewTrackID) };
    this->p_rootFolder()->renumberTrackIDsWith(convertionMap);

    // -- If for some reason the collection stays open, we need to force caches to re-update later.
    this->p_maximumTrackIDFound = newTrackID;
    this->p_maybeRekordboxTracks = nothing;
    this->p_rekordboxTracksPerAbsoluteFilePath.removeAll();
    this->p_rekordboxTracksPerTrackID.removeAll();

    auto volume = this->volume();
    CrashLog::addBreadCrumb(String::stringWithFormat("Tracks for rekordbox collection on volume '%s' (%s) were renumbered.",
                                                     volume.name().asUTF8(), volume.asFilePath().asEncodedString().asUTF8()));

    this->p_tracksWillNeedRenumbering = false;
}

boolean MutableCollection::canReceiveFileAt(const FilePath& filePath, Optional<String>& maybeError) const
{
    if (!filePath.hasExtension(NXA_FILEPATH("xml"), FilePath::CaseSensitivity::None)) {
        return false;
    }

    auto maybeFileContent = File::maybeContentOfFileAtAsString(filePath);
    if (!maybeFileContent.isValid()) {
        return false;
    }

    auto maybeXMLDocument = XMLDocument::maybeWithString(*maybeFileContent);
    if (!maybeXMLDocument.isValid()) {
        return false;
    }

    maybeError = Rekordbox::MutableCollection::maybeErrorIfNotValidRekordboxXML(**maybeXMLDocument);
    return !maybeError.isValid();
}

boolean MutableCollection::receiveFileAt(const FilePath& filePath)
{
    this->reset();

    if (!filePath.hasExtension(NXA_FILEPATH("xml"), FilePath::CaseSensitivity::None)) {
        return false;
    }

    bool openCausedAnError = this->p_openWithFileAt(filePath).isValid();
    if (!openCausedAnError) {
        // -- If we didn't get an error then this collection can be saved on exit.
        this->markAsModifiedNow();
    }

    return !openCausedAnError;
}

Optional<Common::Collection::Error> MutableCollection::open()
{
    return this->p_openWithFileAt(this->p_xmlFilePath);
}

void MutableCollection::notifyUserPreferencesHaveChanged()
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    this->p_artistsSeparator = *userPreferences->maybeStringForKey(String{ NXA_ARTISTS_SEPARATOR_PREFERENCES_KEY });
    this->p_genresSeparator = *userPreferences->maybeStringForKey(String{ NXA_GENRES_SEPARATOR_PREFERENCES_KEY });
    this->p_musicalKeysSeparator = *userPreferences->maybeStringForKey(String{ NXA_MUSICAL_KEYS_SEPARATOR_PREFERENCES_KEY });
    this->p_maximumNumberOfHotCuesToExport = *userPreferences->maybeIntegerForKey(String{ NXA_MAXIMUM_NUMBER_OF_HOTCUES_TO_EXPORT_FOR_REKORDBOX_PREFERENCES_KEY });
    this->p_maximumNumberOfGridMarkersToImport = *userPreferences->maybeIntegerForKey(String{ NXA_NUMBER_OF_REKORDBOX_GRID_MARKERS_BEFORE_IGNORING_THEM_PREFERENCES_KEY });
    this->p_shouldExportHotCuesAlsoAsMemoryCues = *userPreferences->maybeBooleanForKey(String{ NXA_DUPLICATE_REKORDBOX_HOTCUES_AS_MEMORY_CUES_PREFERENCES_KEY });
    this->p_allowsMovieTracksInPlaylists = *userPreferences->maybeBooleanForKey(String{ NXA_ALLOW_REKORDBOX_VIDEO_TRACKS_IN_PLAYLISTS_PREFERENCES_KEY });
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

    auto newNode = this->p_maybeAllTracksPlaylistNode->appendSubNodeNamed("TRACK");

#if defined(NXA_PLATFORM_MACOS)
    // -- On macOS absolute paths already have a leading / because of the root.
    static String prefix{ "file://localhost" };
#elif defined(NXA_PLATFORM_WINDOWS)
    static String prefix{ "file://localhost/" };
#else
    #error Unsupported platform.
#endif
    auto newTrackID = ++this->p_maximumTrackIDFound;

    auto trackLocation = FilePath{ prefix.stringByAppending(absoluteFilePath.asEncodedString()) };
    newNode.setCountValueForAttributeNamed(newTrackID, "TrackID");
    newNode.setStringValueForAttributeNamed(trackLocation.asEncodedString().asStringByAddingPercentEncoding(), "Location");

    auto& tracks = *this->p_maybeRekordboxTracks;
    tracks.append(Shared<MutableTrack>::with(newNode, this, MutableTrack::p_isProtected));

    auto lastTrackAdded = tracks.lastObject().asRawPointer();
    NXA_ASSERT_TRUE(this->p_rekordboxTracksPerAbsoluteFilePath.setValueForKeyCausedAnInsertion(lastTrackAdded, absoluteFilePath));

    lastTrackAdded->setTrackID(newTrackID);
    this->p_rekordboxTracksPerTrackID.setValueForKey(lastTrackAdded, newTrackID);

    this->markAllTracksAsModifiedNow();

    return lastTrackAdded;
}

Optional<NotNull<const Common::Track*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath) const
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return this->p_rekordboxTracksPerAbsoluteFilePath.maybeValueForKey(absoluteFilePath).maybe([](auto&& track) {
        return NotNull<const Common::Track*>{ track };
    });
}

Optional<NotNull<Common::MutableTrack*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return this->p_rekordboxTracksPerAbsoluteFilePath.maybeValueForKey(absoluteFilePath).maybe([](auto&& track) {
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
        this->p_rekordboxTracksPerTrackID.removeValueForKey(*maybeTrackID);
    }

    this->p_rekordboxTracksPerAbsoluteFilePath.removeValueForKey(existingTrack->absoluteFilePath());

    this->p_maybeAllTracksPlaylistNode->deleteSubNodeNamedAtIndex("TRACK", index);

    tracks.removeObjectAtIndex(index);

    this->markAsModifiedNow();
}

void MutableCollection::removeAllTracks()
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_maybeAllTracksPlaylistNode->deleteSubNodesNamed("TRACK");
    this->p_maybeRekordboxTracks = MutableArray<Shared<MutableTrack>>{ };
    this->p_rekordboxTracksPerTrackID.removeAll();
    this->p_rekordboxTracksPerAbsoluteFilePath.removeAll();
    this->p_maximumTrackIDFound = 0;

    this->markAsModifiedNow();
}
