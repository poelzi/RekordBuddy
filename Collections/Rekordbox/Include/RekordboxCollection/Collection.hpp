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

#include <RekordboxCollection/Crates/Folder.hpp>
#include <RekordboxCollection/Crates/AllTracksPlaylist.hpp>
#include <RekordboxCollection/Tracks/Track.hpp>

#include <CommonCollection/Collection.hpp>
#include <CommonCollection/Tracks/Track.hpp>
#include <CommonCollection/Tracks/TrackPredicate.hpp>
#include <CommonCollection/Tracks/TrackPredicateInspector.hpp>
#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Artist.hpp>
#include <CommonCollection/Tracks/Tag.hpp>

#include <Base/TestUtility.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Rekordbox {

// -- Public Interface
class MutableCollection : public Common::MutableCollection, public Common::Collection
{
    // -- Private Class Methods
    static Optional<MutableXMLNode> p_maybeDJPlaylistsNodeForDocument(XMLDocument&);
    static Optional<MutableXMLNode> p_maybeProductNodeForDocument(XMLDocument&);

    // -- Private Instance Variables
    FilePath p_xmlFilePath;
    boolean p_isAtAUserLocation;

    Optional<Shared<XMLDocument>> p_maybeXMLDocument;
    Optional<MutableXMLNode> p_maybeAllTracksPlaylistNode;
    mutable Optional<Unique<MutableFolder>> p_maybeRootFolder;
    mutable Optional<Unique<MutableAllTracksPlaylist>> p_maybeAllTracksPlaylist;

    mutable Optional<String> p_maybeOpenErrorDescription;

    Time p_lastModificationTime;
    Time p_allTracksLastModificationTime;
    Time p_playlistsLastModificationTime;
    Time p_lastSavingTime;

    mutable count p_maximumTrackIDFound;
    mutable boolean p_tracksWillNeedRenumbering;

    mutable Optional<MutableArray<Shared<MutableTrack>>> p_maybeRekordboxTracks;
    mutable MutableMap<FilePath, NotNull<MutableTrack*>> p_rekordboxTracksPerAbsoluteFilePath;
    mutable MutableMap<count, NotNull<MutableTrack*>> p_rekordboxTracksPerTrackID;

    String p_artistsSeparator;
    String p_genresSeparator;
    String p_musicalKeysSeparator;
    count p_maximumNumberOfHotCuesToExport;
    count p_maximumNumberOfGridMarkersToImport;
    boolean p_shouldExportHotCuesAlsoAsMemoryCues;
    boolean p_allowsMovieTracksInPlaylists;

    Optional<Collection::Error> p_lastOpenResult;

    // -- Private Instance Methods
    String p_description() const override
    {
        return this->p_xmlFilePath.asEncodedString();
    }

    Optional<Common::Collection::Error> p_openWithFileAt(const FilePath&);

    Optional<MutableXMLNode> p_maybeDJPlaylistsNode() const;
    Optional<MutableXMLNode> p_maybeProductNode() const;
    boolean p_hasAValidRekordboxXML() const;

    NotNull<Rekordbox::MutableFolder*> p_rootFolder() const;
    NotNull<Rekordbox::MutableAllTracksPlaylist*> p_tracks() const;

    void p_renumberTrackIDs();
    void p_parseTracksIfNeeded() const
    {
        if (!this->p_maybeRekordboxTracks.isValid()) {
            NXA_ASSERT_TRUE(this->isOpened());

            this->p_maybeRekordboxTracks = MutableArray<Shared<MutableTrack>>{ };

            auto& tracks = *this->p_maybeRekordboxTracks;
            auto mutatedThis = const_cast<MutableCollection*>(this);

            boolean collectionWasCorrected = false;
            count index = 0;
            count lastTrackID = 0;

            for (auto&& xmlNode : mutatedThis->p_maybeAllTracksPlaylistNode->subNodesNamed("TRACK")) {
                auto maybeNewTrack = MutableTrack::maybeTrackWithNodeInCollection(xmlNode, mutatedThis, MutableTrack::p_isProtected);
                if (!maybeNewTrack.isValid()) {
                    ++index;
                    continue;
                }

                auto lastTrackAdded = maybeNewTrack->asRawPointer();
                auto absoluteFilePath = lastTrackAdded->absoluteFilePath();
                if (this->p_rekordboxTracksPerAbsoluteFilePath.setValueForKeyIfNotAlreadySet(lastTrackAdded, absoluteFilePath)) {
                    auto maybeTrackID = lastTrackAdded->maybeTrackID();
                    if (maybeTrackID.isValid()) {
                        // -- Maybe not all rekordbox tracks will have track IDs if playlists only use filepaths as key
                        auto& trackID = *maybeTrackID;
                        if (trackID > this->p_maximumTrackIDFound) {
                            this->p_maximumTrackIDFound = trackID;
                        }

                        if (trackID < lastTrackID) {
                            this->p_tracksWillNeedRenumbering = true;
                        }

                        lastTrackID = trackID;

                        this->p_rekordboxTracksPerTrackID.setValueForKey(lastTrackAdded, trackID);
                    }

                    tracks.append(*maybeNewTrack);

                    ++index;
                }
                else {
                    mutatedThis->p_maybeAllTracksPlaylistNode->deleteSubNodeNamedAtIndex("TRACK", index);
                    collectionWasCorrected = true;
                }
            }

            if (this->p_tracksWillNeedRenumbering) {
                collectionWasCorrected = true;
            }

            if (collectionWasCorrected) {
                mutatedThis->markAllTracksAsModifiedNow();
            }
        }
    }
    const MutableArray<Shared<MutableTrack>>& p_ensureTracksAreParsed() const
    {
        this->p_parseTracksIfNeeded();
        return *this->p_maybeRekordboxTracks;
    }
    MutableArray<Shared<MutableTrack>>& p_ensureTracksAreParsed()
    {
        this->p_parseTracksIfNeeded();
        return *this->p_maybeRekordboxTracks;
    }

public:
    // -- Class Methods
    static Optional<String> maybeErrorIfNotValidRekordboxXML(XMLDocument&);

    // -- Constructors & Destructors
    MutableCollection(const FilePath&, boolean isAtAUserLocation = false);
    ~MutableCollection() override = default;

    // -- Instance Methods
    inline FilePath xmlFilePath() const
    {
        return this->p_xmlFilePath;
    }
    inline boolean isAtAUserLocation() const
    {
        return this->p_isAtAUserLocation;
    }
    inline NXA_VIRTUAL_FOR_TESTING boolean shouldExportHotCuesAlsoAsMemoryCues() const
    {
        return this->p_shouldExportHotCuesAlsoAsMemoryCues;
    }
    inline NXA_VIRTUAL_FOR_TESTING count maximumNumberOfHotCuesToExport() const
    {
        return this->p_maximumNumberOfHotCuesToExport;
    }
    inline NXA_VIRTUAL_FOR_TESTING count maximumNumberOfGridMarkersToImport() const
    {
        return this->p_maximumNumberOfGridMarkersToImport;
    }
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
        return this->p_allowsMovieTracksInPlaylists;
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
        return this->isAtAUserLocation() ? String{ "rekordbox (user location)" }
                                         : String{ "rekordbox" } ;
    }
    String fullName() const override
    {
        return this->name();
    }
    const character* iconName() const override
    {
        return "Preferences/Rekordbox Logo/rekordbox 256";
    }
    Common::Collection::Type type() const override
    {
        return Common::Collection::Type::rekordbox;
    }
    Volume volume() const override
    {
        return Volume{ this->p_xmlFilePath };
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
        return this->p_maybeAllTracksPlaylistNode.isValid();
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
        this->p_maybeXMLDocument = nothing;
        this->p_maybeAllTracksPlaylistNode = nothing;
        this->p_maybeRootFolder = nothing;
        this->p_maybeAllTracksPlaylist = nothing;

        this->p_maybeOpenErrorDescription = nothing;

        this->p_lastModificationTime = Time::distantPast();
        this->p_allTracksLastModificationTime = Time::distantPast();
        this->p_playlistsLastModificationTime = Time::distantPast();
        this->p_lastSavingTime = Time::distantPast();

        this->p_tracksWillNeedRenumbering = false;
        this->p_maximumTrackIDFound = 0;

        this->p_maybeRekordboxTracks = nothing;
        this->p_rekordboxTracksPerAbsoluteFilePath.removeAll();
        this->p_rekordboxTracksPerTrackID.removeAll();

        this->p_artistsSeparator = String{ };
        this->p_genresSeparator = String{ };
        this->p_musicalKeysSeparator = String{ };
        this->p_maximumNumberOfHotCuesToExport = 0;
        this->p_maximumNumberOfGridMarkersToImport = 0;
        this->p_shouldExportHotCuesAlsoAsMemoryCues = false;
        this->p_allowsMovieTracksInPlaylists = false;

        this->p_lastOpenResult = nothing;
    }
    void save() override
    {
        NXA_ASSERT_TRUE(this->isOpened());
        NXA_ASSERT_TRUE(this->p_maybeXMLDocument.isValid());

        if (this->p_tracksWillNeedRenumbering) {
            this->p_renumberTrackIDs();
        }

        File::writeStringToFileAt((*this->p_maybeXMLDocument)->asString(), this->p_xmlFilePath);
        this->p_lastSavingTime = this->p_lastModificationTime = Time::currentTime();

        File::setModificationTimeForFile(this->p_lastSavingTime, this->p_xmlFilePath);
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
        this->p_maybeAllTracksPlaylistNode->moveNodesNamedAtIndicesToIndex("TRACK", indices, toIndex);

        this->markAllTracksAsModifiedNow();
        this->p_tracksWillNeedRenumbering = true;
    }
    void moveTrackAtIndexTo(count index, count toIndex)
    {
        this->p_ensureTracksAreParsed().moveObjectAtIndexTo(index, toIndex);
        this->p_maybeAllTracksPlaylistNode->moveNodeNamedAtIndexTo("TRACK", index, toIndex);

        this->markAllTracksAsModifiedNow();
        this->p_tracksWillNeedRenumbering = true;
    }

    NXA_VIRTUAL_FOR_TESTING Optional<NotNull<const Common::Track*>> maybeExistingTrackForTrackID(count trackID) const
    {
        this->p_ensureTracksAreParsed();

        return this->p_rekordboxTracksPerTrackID.maybeValueForKey(trackID).maybe([](auto&& track) {
            return NotNull<const Common::Track*>{ track };
        });
    }
    NXA_VIRTUAL_FOR_TESTING Optional<NotNull<Common::MutableTrack*>> maybeExistingTrackForTrackID(count trackID)
    {
        this->p_ensureTracksAreParsed();

        return this->p_rekordboxTracksPerTrackID.maybeValueForKey(trackID).maybe([](auto&& track) {
            return NotNull<Common::MutableTrack*>{ track };
        });
    }

    NXA_VIRTUAL_FOR_TESTING Time lastAllTracksModificationTime() const
    {
        return this->p_allTracksLastModificationTime;
    }
    NXA_VIRTUAL_FOR_TESTING Time lastPlaylistsModificationTime() const
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
