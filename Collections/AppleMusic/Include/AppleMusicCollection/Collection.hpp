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

#include <AppleMusicCollection/Crates/Folder.hpp>
#include <AppleMusicCollection/Crates/AllTracksPlaylist.hpp>
#include <AppleMusicCollection/Tracks/Track.hpp>

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

namespace NxA { namespace AppleMusic {

// -- Public Interface
class Collection : public Common::Collection
{
    // -- Private Class Methods
    static Optional<XMLNode> p_maybePListNodeForDocument(NotNull<const XMLDocument*>);
    static Optional<XMLNode> p_maybeTopDictNodeForDocument(NotNull<const XMLDocument*>);
    static boolean p_isAValidAndSupportedITunesXML(NotNull<const XMLDocument*>, Optional<XMLNode>&, Optional<XMLNode>&);

    // -- Private Instance Variables
    FilePath p_xmlFilePath;
    boolean p_isAtAUserLocation;

    Optional<Shared<XMLDocument>> p_maybeXMLDocument;
    mutable Optional<XMLNode> p_maybeAllTracksPlaylistNode;
    mutable Optional<XMLNode> p_maybePlaylistsNode;

    mutable MutableMap<String, XMLNode> p_xmlNodesPerPersistentID;
    mutable MutableMap<String, MutableArray<String>> p_persistentIDsPerParentPersistentID;
    mutable MutableArray<String> p_rootPersistentIDs;
    mutable MutableMap<String, String> p_playlistNamePerPersistentID;

    mutable Optional<Unique<Folder>> p_maybeRootFolder;
    mutable Optional<Unique<AllTracksPlaylist>> p_maybeAllTracksPlaylist;

    mutable Optional<String> p_maybeOpenErrorDescription;

    Time p_lastModificationTime = Time::distantPast();

    mutable Optional<MutableArray<Shared<Track>>> p_maybeITunesTracks;
    mutable MutableMap<count, NotNull<Track*>> p_appleMusicTracksPerTrackID;

    String p_artistsSeparator;
    String p_genresSeparator;
    String p_musicalKeysSeparator;

    Optional<Common::Collection::Error> p_lastOpenResult;

    // -- Private Instance Methods
    String p_description() const override
    {
        return this->volume().name();
    }

    Optional<XMLNode> p_maybePListNode() const;
    Optional<XMLNode> p_maybeTopDictNode() const;

    NotNull<AppleMusic::Folder*> p_rootFolder() const;
    NotNull<AppleMusic::AllTracksPlaylist*> p_tracks() const;

    const MutableArray<Shared<Track>>& p_ensureTracksAreParsed() const;

public:
    // -- Class Methods
    static boolean isAValidAndSupportedITunesXML(XMLDocument&);

    // -- Constructors & Destructors
    Collection(const FilePath& xmlFilePath,
               boolean isAtAUserLocation = false) : p_xmlFilePath(xmlFilePath),
                                                    p_isAtAUserLocation(isAtAUserLocation) { }
    ~Collection() override = default;

    // -- Instance Methods
    inline FilePath xmlFilePath() const
    {
        return this->p_xmlFilePath;
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
    inline boolean providesCuePointsAndBeatGrid() const override
    {
        return false;
    }
    Optional<Common::Collection::Error> open() override;
    Optional<Collection::Error> lastOpenResult() const override
    {
        return this->p_lastOpenResult;
    }
    String name() const override
    {
        return this->isAtAUserLocation() ? String{ "iTunes (user location)" }
                                         : String{ "iTunes" } ;
    }
    String fullName() const override
    {
        return this->name();
    }
    const character* iconName() const override
    {
        return "Preferences/iTunes Logo/iTunes256";
    }
    Common::Collection::Type type() const override
    {
        return Common::Collection::Type::iTunes;
    }
    Volume volume() const override
    {
        return Volume{ this->p_xmlFilePath };
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
    NotNull<const Common::Playlist*> tracks() const override;
    Array<Unique<Common::Artist>> artists() const override
    {
        return { };
    }
    Array<Unique<Common::MusicalKey>> musicalKeys() const override
    {
        return { };
    }
    Array<Unique<Common::Tag>> tags() const override
    {
        return { };
    }
    Array<Common::Property::TypeID> propertyTypes() const override
    {
        return { };
    }

    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&) const override;

    Optional<NotNull<const Common::Track*>> maybeExistingTrackWithRelativeFilePath(const FilePath&) const override;

    count numberOfTracks() const
    {
        return this->p_ensureTracksAreParsed().length();
    }
    NotNull<const Common::Track*> trackAtIndex(count index) const
    {
        return this->p_ensureTracksAreParsed()[index].asRawPointer();
    }

    Optional<Array<String>> maybeChildrenForPersistentID(const String& persistentID) const
    {
        auto maybeIDs = this->p_persistentIDsPerParentPersistentID.maybeValueForKey(persistentID);
        if (!maybeIDs.isValid()) {
            return nothing;
        }

        return { Array<String>{ std::move(*maybeIDs) } };
    }
    Optional<XMLNode> maybePlaylistForPersistentID(const String& persistentID) const
    {
        return this->p_xmlNodesPerPersistentID.maybeValueForKey(persistentID);
    }
    Optional<String> maybePlaylistNameForPersistentID(const String& persistentID) const
    {
        return this->p_playlistNamePerPersistentID.maybeValueForKey(persistentID);
    }

    NXA_VIRTUAL_FOR_TESTING Optional<NotNull<const Common::Track*>> maybeExistingTrackForTrackID(count trackID) const
    {
        this->p_ensureTracksAreParsed();

        return this->p_appleMusicTracksPerTrackID.maybeValueForKey(trackID).maybe([](auto&& track) {
            return NotNull<const Common::Track*>{ track };
        });
    }
};

} }
