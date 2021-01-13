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

#include <EngineCollection/Collection.hpp>
#include <EngineCollection/Tracks/Track.hpp>
#include <EngineCollection/Crates/Folder.hpp>
#include <EngineCollection/Crates/AllTracksPlaylist.hpp>
#include <EngineCollection/Engine.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

using namespace NxA;
using namespace NxA::Engine;

// -- Constructors & Destructors

MutableCollection::MutableCollection()
{
    this->reset();
}

// -- Instance Methods

Optional<Common::Collection::Error> MutableCollection::p_openWithFileAt(const FilePath& collectionFilePath)
{
    if (this->isOpened()) {
        return nothing;
    }

    // -- To be implemented.
    this->p_lastOpenResult = Common::Collection::Error::CannotOpen;
    this->p_lastModificationTime = Time::currentTime();

    this->p_playlistsLastModificationTime = this->p_lastModificationTime;
    this->p_allTracksLastModificationTime = this->p_lastModificationTime;
    this->p_lastSavingTime = this->p_lastModificationTime;

    this->notifyUserPreferencesHaveChanged();

    return this->p_lastOpenResult = nothing;
}

NotNull<Engine::MutableFolder*> MutableCollection::p_rootFolder() const
{
    if (!this->p_maybeRootFolder.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

    }

    NXA_ALOG("To be implemented.");
}

NotNull<Engine::MutableAllTracksPlaylist*> MutableCollection::p_tracks() const
{
    if (!this->p_maybeAllTracksPlaylist.isValid()) {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeAllTracksPlaylist = Unique<MutableAllTracksPlaylist>::with(const_cast<MutableCollection*>(this), MutableAllTracksPlaylist::p_isProtected);
    }

    return this->p_maybeAllTracksPlaylist->asRawPointer();
}

boolean MutableCollection::canReceiveFileAt(const FilePath& filePath, Optional<String>& maybeError) const
{
    // -- To be implemented
    return false;
}

boolean MutableCollection::receiveFileAt(const FilePath& filePath)
{
    this->reset();

    bool openCausedAnError = this->p_openWithFileAt(filePath).isValid();
    if (!openCausedAnError) {
        // -- If we didn't get an error then this collection can be saved on exit.
        this->markAsModifiedNow();
    }

    return !openCausedAnError;
}

Optional<Common::Collection::Error> MutableCollection::open()
{
    // -- To be implemented
    return this->p_openWithFileAt(FilePath{ });
}

void MutableCollection::notifyUserPreferencesHaveChanged()
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::homeVolume());

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

    this->markAllTracksAsModifiedNow();

    NXA_ALOG("Not implemented yet.");
    //return lastTrackAdded;
}

Optional<NotNull<const Common::Track*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath) const
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return { };
}

Optional<NotNull<Common::MutableTrack*>> MutableCollection::maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath)
{
    NXA_ASSERT_TRUE(this->isOpened());

    this->p_ensureTracksAreParsed();

    return { };
}

void MutableCollection::removeTrackAtIndex(count index)
{
    NXA_ASSERT_TRUE(this->isOpened());

    auto& tracks = this->p_ensureTracksAreParsed();

    tracks.removeObjectAtIndex(index);

    this->markAsModifiedNow();
}

void MutableCollection::removeAllTracks()
{
    NXA_ASSERT_TRUE(this->isOpened());


    this->markAsModifiedNow();
}
