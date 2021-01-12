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

#include <CommonCollection/Collection.hpp>
#include <CommonCollection/Tracks/Property.hpp>
#include <CommonCollection/Tracks/Track.hpp>
#include <CommonCollection/Crates/CratePath.hpp>
#include <CommonCollection/Crates/SubCrate.hpp>

#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Time.hpp>
#include <Base/Types.hpp>
#include <Base/Uncopyable.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class Tag;
class MutableTag;
class Track;
class MutableTrack;
class TrackPredicate;

// -- Public Interface
class Playlist : public Uncopyable
{
public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const Playlist& first, const T& second)
        {
            return (first.collection() == second.collection()) && (first.path() == second.path());
        }
    template <class T>
        inline static boolean isLessThan(const Playlist& first, const T& second)
        {
            return (first.collection() < second.collection()) && (first.path() < second.path());
        }

    template <class TrackPointerType, class T>
        static Array<count> indicesForTrackIn(NotNull<TrackPointerType*> track, T& playlist)
        {
            NXA_ASSERT_TRUE(track->collection() == playlist.collection());
            MutableArray<count> indices;

            for (count index = 0; index < playlist.numberOfTracks(); ++index) {
                if (playlist.trackAtIndex(index) == track) {
                    indices.append(index);
                }
            }

            return std::move(indices);
        }

    template <class TrackPointerType, class T>
        static Array<TrackPointerType> tracksIn(T& playlist)
        {
            MutableArray<TrackPointerType> results;

            for (count index = 0; index < playlist.numberOfTracks(); ++index) {
                results.append(playlist.trackAtIndex(index));
            }

            return { std::move(results) };
        }

    template <typename T>
        static count numberOfCratesAndTrackEntriesContainedIn(const T& playlist)
        {
            // -- The crate always counts itself.
            count numberOfCratesAndTrackEntriesContainedWithin = 1;

            auto numberOfSubCrates = playlist.numberOfSubCrates();
            if (numberOfSubCrates) {
                for (count index = 0; index < numberOfSubCrates; ++index) {
                    numberOfCratesAndTrackEntriesContainedWithin += playlist.subCrateAtIndex(index).apply([](auto& subCrate) {
                        return subCrate->numberOfCratesAndTrackEntriesContainedWithin();
                    });
                }
            }
            else {
                numberOfCratesAndTrackEntriesContainedWithin += playlist.numberOfTracks();
            }

            return numberOfCratesAndTrackEntriesContainedWithin;
        }

    template <typename T>
        static count hasTracksNotOnVolume(const T& playlist, const Volume& volume)
        {
            for (auto&& track : playlist.tracks()) {
                if (track->volume() != volume) {
                    return true;
                }
            }

            return false;
        }
    template <typename T>
        static count hasMovieTracks(const T& playlist)
        {
            for (auto&& track : playlist.tracks()) {
                if (Track::isAMovieTrack(*track)) {
                    return true;
                }
            }

            return false;
        }

    // -- Constructors & Destructors
    virtual ~Playlist() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual Optional<NotNull<const Folder*>> maybeParentFolder() const = 0;

    virtual Time lastModificationTime() const = 0;
    virtual String name() const = 0;
    virtual CratePath path() const = 0;
    virtual const character* iconName() const = 0;

    count numberOfCratesAndTrackEntriesContainedWithin() const;
    boolean hasTracksNotOnVolume(const Volume&) const;
    boolean hasMovieTracks() const;

    virtual count numberOfSubCrates() const = 0;
    virtual SubCrate subCrateAtIndex(count) const = 0;

    virtual count numberOfTracks() const = 0;
    virtual NotNull<const Track*> trackAtIndex(count) const = 0;
    virtual Array<NotNull<const Track*>> tracks() const = 0;

    virtual boolean isOrganizedBy(const Tag&) const = 0;
    virtual boolean isOrganizedBy(Property::TypeID) const = 0;
};

class MutablePlaylist : public Uncopyable
{
public:
    // -- Constants
    enum class AndUpdateTracks {
        No,
        Yes
    };

private:
    // -- Private Instance Methods
    template<class T>
        Optional<count> p_addTrackAtIndex(NotNull<T*>, count, AndUpdateTracks, Common::Track::TrackIsOnSameVolume);

protected:
    // -- Protected Instance Methods
    virtual Optional<count> p_addExistingTrackAtIndex(NotNull<MutableTrack*>, count)
    {
        NXA_ALOG("Can't add tracks to this type of playlist.");
    }

public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const MutablePlaylist& first, const T& second)
        {
            return (first.collection() == second.collection()) && (first.path() == second.path());
        }
    template <class T>
        inline static boolean isLessThan(const MutablePlaylist& first, const T& second)
        {
            return (first.collection() < second.collection()) && (first.path() < second.path());
        }

    // -- Constructors & Destructors
    virtual ~MutablePlaylist() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual Optional<NotNull<const Folder*>> maybeParentFolder() const = 0;
    virtual Optional<NotNull<MutableFolder*>> maybeParentFolder() = 0;

    virtual boolean canBeCloned() const
    {
        return false;
    }
    virtual NotNull<MutablePlaylist*> cloneWithName(const String&)
    {
        NXA_ALOG("Cannot be cloned.");
    }

    virtual Time lastModificationTime() const = 0;
    virtual String name() const = 0;
    virtual void setName(const String&) = 0;
    virtual CratePath path() const = 0;
    virtual const character* iconName() const = 0;

    String nextAvailableNameForThisIfAddedTo(const MutableFolder&) const;

    count numberOfCratesAndTrackEntriesContainedWithin() const;
    boolean hasTracksNotOnVolume(const Volume&) const;
    boolean hasMovieTracks() const;

    virtual count numberOfSubCrates() const = 0;
    virtual SubCrate subCrateAtIndex(count) const = 0;
    virtual MutableSubCrate subCrateAtIndex(count) = 0;

    virtual count numberOfTracks() const = 0;
    virtual NotNull<const Track*> trackAtIndex(count) const = 0;
    virtual NotNull<MutableTrack*> trackAtIndex(count) = 0;
    virtual Array<NotNull<const Track*>> tracks() const = 0;
    virtual Array<NotNull<MutableTrack*>> tracks() = 0;
    virtual boolean canReceive(NotNull<const Track*>) const = 0;
    virtual boolean canReceive(NotNull<MutableTrack*>) const = 0;
    Optional<count> addTrackAtIndex(NotNull<const Track*>,
                                    count,
                                    AndUpdateTracks,
                                    Track::TrackIsOnSameVolume = Track::TrackIsOnSameVolume::DontKnow);
    Optional<count> addTrackAtIndex(NotNull<MutableTrack*>,
                                    count,
                                    AndUpdateTracks,
                                    Track::TrackIsOnSameVolume = Track::TrackIsOnSameVolume::DontKnow);
    virtual void moveTracksAtIndicesToIndex(Array<count>, count) = 0;
    virtual void moveTrackAtIndexTo(count, count) = 0;
    void removeTrackWithRelativeFilePath(const FilePath&);
    void removeTrackWithAbsoluteFilePath(const FilePath&);
    virtual void removeTrackAtIndex(count) = 0;
    virtual void removeAllTracks() = 0;

    virtual boolean isOrganizedBy(const Tag&) const = 0;
    virtual void organizeBy(MutableTag&) = 0;
    virtual void removeOrganizationBy(MutableTag&) = 0;

    virtual boolean isOrganizedBy(Property::TypeID) const = 0;
    virtual void organizeBy(Property::TypeID typeID) = 0;
    virtual void removeOrganizationBy(Property::TypeID typeID) = 0;

    template <class T>
        void setWithSameTracksAsWithPerItemProgressCallBack(const T& other,
                                                            const std::function<void(void)>& callback,
                                                            AndUpdateTracks andUpdateTracks)
        {
            auto collection = this->collection();

            this->removeAllTracks();

            auto thisMustHaveTracksOnSameVolume = collection->mustHaveTracksOnTheSameVolume();
            auto otherHasTracksOnSameVolume = other.collection()->mustHaveTracksOnTheSameVolume();
            auto thisVolume = collection->volume();

            auto weAreAlwaysOnTheSameVolume = (thisMustHaveTracksOnSameVolume && otherHasTracksOnSameVolume && (thisVolume == other.collection()->volume()));

            count thisTrackIndex = 0;
            for (count otherTrackIndex = 0; otherTrackIndex < other.numberOfTracks();) {
                auto otherTrack = other.trackAtIndex(otherTrackIndex++);
                auto weAreOnTheSameVolume = weAreAlwaysOnTheSameVolume || (otherTrack->volume() == thisVolume);

                if (thisMustHaveTracksOnSameVolume && !weAreOnTheSameVolume) {
                    // -- Other track is not on the same volume as this one so we skip it.
                    continue;
                }

                auto receivingAtIndex = std::min(thisTrackIndex++, this->numberOfTracks());
                this->addTrackAtIndex(otherTrack,
                                      receivingAtIndex,
                                      andUpdateTracks,
                                      weAreOnTheSameVolume ? Track::TrackIsOnSameVolume::Yes : Track::TrackIsOnSameVolume::DontKnow);

#if defined(NXA_DEBUG_PROGRESS_CALLBACK)
                NXA_DLOG_WITH_FORMAT("Adding Track entry in '%s' for '%s'\n",
                                     this->path().asString().asUTF8(),
                                     otherTrack->absoluteFilePath().asEncodedString().asUTF8());
#endif
                callback();
            }
        }
};

} }
