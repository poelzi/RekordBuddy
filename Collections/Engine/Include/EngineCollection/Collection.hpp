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

#pragma once

#include <EngineCollection/Crates/Folder.hpp>
#include <EngineCollection/Crates/AllTracksPlaylist.hpp>
#include <EngineCollection/Tracks/Track.hpp>

#include <CommonCollection/Collection.hpp>
#include <CommonCollection/Tracks/Track.hpp>
#include <CommonCollection/Tracks/TrackPredicate.hpp>
#include <CommonCollection/Tracks/TrackPredicateInspector.hpp>
#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Artist.hpp>
#include <CommonCollection/Tracks/Tag.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Engine {

// -- Public Interface
class MutableCollection : public Common::MutableCollection, public Common::Collection
{
    // -- Private Instance Variables
    mutable Optional<Unique<MutableFolder>> p_maybeRootFolder;
    mutable Optional<Unique<MutableAllTracksPlaylist>> p_maybeAllTracksPlaylist;

    mutable Optional<String> p_maybeOpenErrorDescription;

    Time p_lastModificationTime;
    Time p_allTracksLastModificationTime;
    Time p_playlistsLastModificationTime;
    Time p_lastSavingTime;

    mutable count p_maximumTrackIDFound;

    mutable Optional<MutableArray<Shared<MutableTrack>>> p_maybeTracks;
    mutable MutableMap<FilePath, NotNull<MutableTrack*>> p_tracksPerAbsoluteFilePath;
    mutable MutableMap<count, NotNull<MutableTrack*>> p_tracksPerTrackID;

    String p_artistsSeparator;
    String p_genresSeparator;
    String p_musicalKeysSeparator;

    Optional<Collection::Error> p_lastOpenResult;

    // -- Private Instance Methods
    String p_description() const override
    {
        return { };
    }

    Optional<Common::Collection::Error> p_openWithFileAt(const FilePath&);

    NotNull<Engine::MutableFolder*> p_rootFolder() const;
    NotNull<Engine::MutableAllTracksPlaylist*> p_tracks() const;

    void p_parseTracksIfNeeded() const
    {
        if (!this->p_maybeTracks.isValid()) {
            NXA_ASSERT_TRUE(this->isOpened());

            this->p_maybeTracks = MutableArray<Shared<MutableTrack>>{ };

            //auto& tracks = *this->p_maybeTracks;
            //auto mutatedThis = const_cast<MutableCollection*>(this);
        }
    }
    const MutableArray<Shared<MutableTrack>>& p_ensureTracksAreParsed() const
    {
        this->p_parseTracksIfNeeded();
        return *this->p_maybeTracks;
    }
    MutableArray<Shared<MutableTrack>>& p_ensureTracksAreParsed()
    {
        this->p_parseTracksIfNeeded();
        return *this->p_maybeTracks;
    }

public:
    // -- Constructors & Destructors
    MutableCollection();
    ~MutableCollection() override = default;

    // -- Instance Methods
    inline boolean shouldBeOpenedLazily() const override
    {
        return true;
    }
    inline boolean mustHaveTracksOnTheSameVolume() const override
    {
        return false;
    }
    inline boolean allowsMovieTracksInPlaylists() const override
    {
        return true;
    }
    inline boolean canReceiveFiles() const override
    {
        return true;
    }
    boolean canReceiveFileAt(const FilePath&, Optional<String>&) const override;
    boolean receiveFileAt(const FilePath&) override;

    Optional<Common::Collection::Error> open() override;
    Optional<Collection::Error> lastOpenResult() const override
    {
        return this->p_lastOpenResult;
    }
    String name() const override
    {
        return String{ "Engine" } ;
    }
    String fullName() const override
    {
        return this->name();
    }
    const character* iconName() const override
    {
        return "Preferences/Engine/Engine 256";
    }
    Common::Collection::Type type() const override
    {
        return Common::Collection::Type::Engine;
    }
    Volume volume() const override
    {
        return Volume{ };
    }

    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
    }
    void markAsModifiedNow() override
    {
        this->p_lastModificationTime = Time::currentTime();
    }

    inline boolean isOpened() const override
    {
        return this->p_lastOpenResult.isValid();
    }
    Optional<String> maybeOpeningErrorDescription() const override
    {
        return this->p_maybeOpenErrorDescription;
    }
    boolean hasChangesToSave() const override
    {
        return this->p_lastModificationTime > this->p_lastSavingTime;
    }
    void reset() override
    {
        this->p_maybeRootFolder = nothing;
        this->p_maybeAllTracksPlaylist = nothing;

        this->p_maybeOpenErrorDescription = nothing;

        this->p_lastModificationTime = Time::distantPast();
        this->p_allTracksLastModificationTime = Time::distantPast();
        this->p_playlistsLastModificationTime = Time::distantPast();
        this->p_lastSavingTime = Time::distantPast();

        this->p_maximumTrackIDFound = 0;

        this->p_maybeTracks = nothing;
        this->p_tracksPerAbsoluteFilePath.removeAll();
        this->p_tracksPerTrackID.removeAll();

        this->p_artistsSeparator = String{ };
        this->p_genresSeparator = String{ };
        this->p_musicalKeysSeparator = String{ };

        this->p_lastOpenResult = nothing;
    }
    void save() override
    {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_lastSavingTime = this->p_lastModificationTime = Time::currentTime();
    }
    void saveWithProgress(std::function<void(double)>&&) override
    {
        this->save();
    }

    void notifyUserPreferencesHaveChanged() override;
    inline const String& artistsSeparator() const override
    {
        return this->p_artistsSeparator;
    }
    inline const String& genresSeparator() const override
    {
        return this->p_genresSeparator;
    }
    inline const String& musicalKeysSeparator() const override
    {
        return this->p_musicalKeysSeparator;
    }

    NotNull<const Common::Folder*> rootFolder() const override;
    NotNull<Common::MutableFolder*> rootFolder() override;
    NotNull<const Common::Playlist*> tracks() const override;
    NotNull<Common::MutablePlaylist*> tracks() override;
    Array<Unique<Common::Artist>> artists() const override
    {
        return { };
    }
    Array<Unique<Common::MutableArtist>> artists() override
    {
        return { };
    }
    Array<Unique<Common::MusicalKey>> musicalKeys() const override
    {
        return { };
    }
    Array<Unique<Common::MutableMusicalKey>> musicalKeys() override
    {
        return { };
    }
    Array<Unique<Common::Tag>> tags() const override
    {
        return { };
    }
    Array<Unique<Common::MutableTag>> tags() override
    {
        return { };
    }
    Array<Common::Property::TypeID> propertyTypes() const override
    {
        return { };
    }

    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithRelativeFilePath(const FilePath& relativeFilePath) const override
    {
        return Common::Collection::p_maybeExistingTrackWithRelativeFilePathIn(relativeFilePath, *this);
    }
    Optional<NotNull<Common::MutableTrack*>> maybeExistingTrackWithRelativeFilePath(const FilePath& relativeFilePath) override
    {
        return this->Common::MutableCollection::maybeExistingTrackWithRelativeFilePath(relativeFilePath);
    }

    NotNull<Common::MutableTrack*> trackWithAbsoluteFilePath(const FilePath&) override;
    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath& absoluteFilePath) const override;
    Optional<NotNull<Common::MutableTrack*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&) override;

    void removeTrackAtIndex(count);
    void removeAllTracks();

    count numberOfTracks() const
    {
        return this->p_ensureTracksAreParsed().length();
    }
    NotNull<const Common::Track*> trackAtIndex(count index) const
    {
        return this->p_ensureTracksAreParsed()[index].asRawPointer();
    }
    NotNull<Common::MutableTrack*> trackAtIndex(count index)
    {
        return this->p_ensureTracksAreParsed()[index].asRawPointer();
    }
    void moveTracksAtIndicesToIndex(Array<count> indices, count toIndex)
    {
        this->p_ensureTracksAreParsed().moveObjectsAtIndicesTo(indices, toIndex);

        // TODO: Implement

        this->markAllTracksAsModifiedNow();
    }
    void moveTrackAtIndexTo(count index, count toIndex)
    {
        this->p_ensureTracksAreParsed().moveObjectAtIndexTo(index, toIndex);

        // TODO: Implement

        this->markAllTracksAsModifiedNow();
    }

    Optional<NotNull<const Common::Track*>> maybeExistingTrackForTrackID(count trackID) const
    {
        this->p_ensureTracksAreParsed();

        return this->p_tracksPerTrackID.maybeValueForKey(trackID).maybe([](auto&& track) {
            return NotNull<const Common::Track*>{ track };
        });
    }
    Optional<NotNull<Common::MutableTrack*>> maybeExistingTrackForTrackID(count trackID)
    {
        this->p_ensureTracksAreParsed();

        return this->p_tracksPerTrackID.maybeValueForKey(trackID).maybe([](auto&& track) {
            return NotNull<Common::MutableTrack*>{ track };
        });
    }

    Time lastAllTracksModificationTime() const
    {
        return this->p_allTracksLastModificationTime;
    }
    Time lastPlaylistsModificationTime() const
    {
        return this->p_playlistsLastModificationTime;
    }
    void markAllTracksAsModifiedNow()
    {
        this->markAsModifiedNow();

        this->p_allTracksLastModificationTime = Time::currentTime();
    }
    void markPlaylistsAsModifiedNow()
    {
        this->markAsModifiedNow();

        this->p_playlistsLastModificationTime = Time::currentTime();
    }
};

} }
