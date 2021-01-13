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

#include <TraktorCollection/Crates/AllTracksPlaylist.hpp>
#include <TraktorCollection/Crates/Folder.hpp>
#include <TraktorCollection/Tracks/Track.hpp>

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

namespace NxA { namespace Traktor {

// -- Public Interface
class MutableCollection : public Common::MutableCollection, public Common::Collection
{
    // -- Private Class Methods
    static boolean p_entryIsATrackEntry(const MutableXMLNode& entry)
    {
#if 0
        // == A lot of people may have tracks listed as samples so we'll allow this for now.
        if (entry.subNodesNamed("LOOPINFO").length()) {
            return false;
        }
#endif

        if (!entry.subNodesNamed("LOCATION").length()) {
            return false;
        }

        return true;
    }

    // -- Private Instance Variables
    FilePath p_nmlFilePath;
    boolean p_isAtAUserLocation;

    Optional<Shared<XMLDocument>> p_maybeXMLDocument;
    Optional<MutableXMLNode> p_maybeAllTracksPlaylistNode;
    mutable Optional<Unique<MutableFolder>> p_maybeRootFolder;
    mutable Optional<Unique<MutableAllTracksPlaylist>> p_maybeAllTracksPlaylist;

    Optional<String> p_maybeOpenErrorDescription;

    Time p_lastModificationTime;
    Time p_allTracksLastModificationTime;
    Time p_playlistsLastModificationTime;
    Time p_lastSavingTime;

    mutable Optional<MutableArray<Shared<MutableTrack>>> p_maybeTraktorTracks;
    mutable MutableMap<FilePath, NotNull<MutableTrack*>> p_traktorTracksPerAbsoluteFilePath;
    mutable MutableMap<String, FilePath> p_traktorAbsoluteFilePathPerPlaylistPath;

    String p_name;

    String p_artistsSeparator;
    String p_genresSeparator;
    String p_musicalKeysSeparator;
    boolean p_useComment2FieldAsGrouping;
    boolean p_mergeGridMarkersAndHotCues;
    boolean p_readTraktorKeyInsteadOfKeyText;

    Optional<Collection::Error> p_lastOpenResult;

    // -- Private Instance Methods
    String p_description() const override
    {
        return this->p_nmlFilePath.asEncodedString();
    }

    Optional<MutableXMLNode> p_maybeNMLNode() const;
    Optional<MutableXMLNode> p_maybePlaylistsNode() const;
    boolean p_hasAValidAndSupportedTraktorNML() const;

    void p_parseTracksIfNeeded() const
    {
        if (!this->p_maybeTraktorTracks.isValid()) {
            NXA_ASSERT_TRUE(this->isOpened());

            this->p_maybeTraktorTracks = MutableArray<Shared<MutableTrack>>{ };

            auto& tracks = *this->p_maybeTraktorTracks;
            auto mutatedThis = const_cast<MutableCollection*>(this);
            boolean collectionWasCorrected = false;
            count index = 0;

            for (auto&& xmlNode : mutatedThis->p_maybeAllTracksPlaylistNode->subNodesNamed("ENTRY")) {
                if (!MutableCollection::p_entryIsATrackEntry(xmlNode)) {
                    continue;
                }

                auto maybeNewTrack = MutableTrack::maybeTrackWithNodeInCollection(xmlNode, mutatedThis, MutableTrack::p_isProtected);
                if (!maybeNewTrack.isValid()) {
                    continue;
                }

                auto lastTrackAdded = maybeNewTrack->asRawPointer();
                auto absoluteFilePath = lastTrackAdded->absoluteFilePath();
                if (this->p_traktorTracksPerAbsoluteFilePath.setValueForKeyIfNotAlreadySet(lastTrackAdded, absoluteFilePath)) {
                    this->p_traktorAbsoluteFilePathPerPlaylistPath.setValueForKey(absoluteFilePath, lastTrackAdded->trackFilePathAsUsedInTraktorPlaylistEntries());

                    tracks.append(*maybeNewTrack);

                    ++index;
                }
                else {
                    mutatedThis->p_maybeAllTracksPlaylistNode->deleteSubNodeNamedAtIndex("ENTRY", index);
                    collectionWasCorrected = true;
                }
            }

            if (collectionWasCorrected) {
                mutatedThis->markAllTracksAsModifiedNow();
            }
        }
    }
    const MutableArray<Shared<MutableTrack>>& p_ensureTracksAreParsed() const
    {
        this->p_parseTracksIfNeeded();
        return *this->p_maybeTraktorTracks;
    }
    MutableArray<Shared<MutableTrack>>& p_ensureTracksAreParsed()
    {
        this->p_parseTracksIfNeeded();
        return *this->p_maybeTraktorTracks;
    }

    NotNull<Traktor::MutableFolder*> p_rootFolder() const;
    NotNull<Traktor::MutableAllTracksPlaylist*> p_tracks() const;

public:
    // -- Constructors & Destructors
    MutableCollection(const FilePath& nmlFilePath,
                      const String& name,
                      boolean isAtAUserLocation = false) : p_nmlFilePath(nmlFilePath),
                                                           p_isAtAUserLocation(isAtAUserLocation),
                                                           p_name(name)
    {

    }
    ~MutableCollection() override = default;


    // -- Instance Methods
    inline FilePath nmlFilePath() const
    {
        return this->p_nmlFilePath;
    }
    inline boolean isAtAUserLocation() const
    {
        return this->p_isAtAUserLocation;
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
        return true;
    }
    Optional<Common::Collection::Error> open() override;
    Optional<Collection::Error> lastOpenResult() const override
    {
        return this->p_lastOpenResult;
    }

    String name() const override
    {
        return this->isAtAUserLocation() ? "Traktor (user location)"_String
                                         : this->p_name;
    }
    String fullName() const override
    {
        return this->name();
    }
    const character* iconName() const override
    {
        return "Preferences/Traktor Logo/Traktor256";
    }
    Common::Collection::Type type() const override
    {
        return Common::Collection::Type::Traktor;
    }
    Volume volume() const override
    {
        return Volume{ this->p_nmlFilePath };
    }
    Time lastModificationTime() const override
    {
        return this->p_lastModificationTime;
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
        this->p_maybeAllTracksPlaylistNode->moveNodesNamedAtIndicesToIndex("ENTRY", indices, toIndex);

        this->markAllTracksAsModifiedNow();
    }
    void moveTrackAtIndexTo(count index, count toIndex)
    {
        this->p_ensureTracksAreParsed().moveObjectAtIndexTo(index, toIndex);
        this->p_maybeAllTracksPlaylistNode->moveNodeNamedAtIndexTo("ENTRY", index, toIndex);

        this->markAllTracksAsModifiedNow();
    }
    void removeAllTracks()
    {
        NXA_ASSERT_TRUE(this->isOpened());

        this->p_maybeAllTracksPlaylistNode->deleteSubNodesNamed("ENTRY");
        this->p_maybeTraktorTracks = MutableArray<Shared<MutableTrack>>{ };
        this->p_traktorTracksPerAbsoluteFilePath.removeAll();
        this->p_traktorAbsoluteFilePathPerPlaylistPath.removeAll();

        this->markAllTracksAsModifiedNow();
    }
    void removeTrackAtIndex(count index)
    {
        NXA_ASSERT_TRUE(this->isOpened());

        auto& tracks = this->p_ensureTracksAreParsed();
        auto& existingTrack = tracks[index];

        this->p_maybeAllTracksPlaylistNode->deleteSubNodeNamedAtIndex("ENTRY", index);
        this->p_traktorTracksPerAbsoluteFilePath.removeValueForKey(existingTrack->absoluteFilePath());
        this->p_traktorAbsoluteFilePathPerPlaylistPath.removeValueForKey(existingTrack->trackFilePathAsUsedInTraktorPlaylistEntries());

        tracks.removeObjectAtIndex(index);

        this->markAllTracksAsModifiedNow();
    }

    void markAsModifiedNow() override
    {
        this->p_lastModificationTime = Time::currentTime();
    }
    void reset() override
    {
        // -- TODO: Implement this if needed for Traktor.
    }
    void save() override
    {
        NXA_ASSERT_TRUE(this->isOpened());
        NXA_ASSERT_TRUE(this->p_maybeXMLDocument.isValid());

        File::writeStringToFileAt((*this->p_maybeXMLDocument)->asString(), this->p_nmlFilePath);

        this->p_lastSavingTime = this->p_lastModificationTime = Time::currentTime();

        File::setModificationTimeForFile(this->p_lastSavingTime, this->p_nmlFilePath);
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
    NXA_VIRTUAL_FOR_TESTING boolean useComment2FieldAsGrouping() const
    {
        return this->p_useComment2FieldAsGrouping;
    }
    NXA_VIRTUAL_FOR_TESTING boolean mergeGridMarkersAndHotCues() const
    {
        return this->p_mergeGridMarkersAndHotCues;
    }
    NXA_VIRTUAL_FOR_TESTING boolean readTraktorKeyInsteadOfKeyText() const
    {
        return this->p_readTraktorKeyInsteadOfKeyText;
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
    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&) const override;
    Optional<NotNull<Common::MutableTrack*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&) override;

    NXA_VIRTUAL_FOR_TESTING Optional<NotNull<const Common::Track*>> maybeExistingTrackWithPlaylistPath(const String&) const;
    NXA_VIRTUAL_FOR_TESTING Optional<NotNull<Common::MutableTrack*>> maybeExistingTrackWithPlaylistPath(const String&);

    NXA_VIRTUAL_FOR_TESTING Time lastAllTracksModificationTime() const
    {
        return this->p_allTracksLastModificationTime;
    }
    NXA_VIRTUAL_FOR_TESTING Time lastPlaylistsModificationTime() const
    {
        return this->p_playlistsLastModificationTime;
    }
};

} }
