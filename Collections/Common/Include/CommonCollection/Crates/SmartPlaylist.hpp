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
class SmartPlaylist : public Uncopyable
{
public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const SmartPlaylist& first, const T& second)
        {
            return (first.collection() == second.collection()) && (first.path() == second.path());
        }
    template <class T>
        inline static boolean isLessThan(const SmartPlaylist& first, const T& second)
        {
            return (first.collection() < second.collection()) && (first.path() < second.path());
        }

    // -- Constructors & Destructors
    virtual ~SmartPlaylist() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual Optional<NotNull<const Common::Folder*>> maybeParentFolder() const = 0;

    virtual Time lastModificationTime() const = 0;
    virtual String name() const = 0;
    virtual Common::CratePath path() const = 0;
    virtual const character* iconName() const = 0;

    count numberOfCratesAndTrackEntriesContainedWithin() const;
    boolean hasTracksNotOnVolume(const Volume&) const;
    boolean hasMovieTracks() const;

    virtual count numberOfSubCrates() const = 0;
    virtual Common::SubCrate subCrateAtIndex(count) const = 0;

    virtual count numberOfTracks() const = 0;
    virtual NotNull<const Track*> trackAtIndex(count) const = 0;
    virtual Array<NotNull<const Track*>> tracks() const = 0;

    virtual boolean canHavePredicates() const = 0;
    virtual Optional<TrackPredicate> maybePredicate() const = 0;

    virtual boolean isOrganizedBy(const Common::Tag&) const = 0;
    virtual boolean isOrganizedBy(Common::Property::TypeID) const = 0;
};

class MutableSmartPlaylist : public Uncopyable
{
public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const MutableSmartPlaylist& first, const T& second)
        {
            return (first.collection() == second.collection()) && (first.path() == second.path());
        }
    template <class T>
        inline static boolean isLessThan(const MutableSmartPlaylist& first, const T& second)
        {
            return (first.collection() < second.collection()) && (first.path() < second.path());
        }

    // -- Constructors & Destructors
    virtual ~MutableSmartPlaylist() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual Optional<NotNull<const Common::Folder*>> maybeParentFolder() const = 0;
    virtual Optional<NotNull<Common::MutableFolder*>> maybeParentFolder() = 0;

    virtual boolean canBeCloned() const
    {
        return false;
    }
    virtual NotNull<MutableSmartPlaylist*> cloneWithName(const String&)
    {
        NXA_ALOG("Cannot be cloned.");
    }

    virtual Time lastModificationTime() const = 0;
    virtual boolean canBeRenamed() const
    {
        return false;
    }
    virtual String name() const = 0;
    virtual void setName(const String&)
    {
        NXA_ALOG("Cannot be renamed.");
    }
    virtual Common::CratePath path() const = 0;
    virtual const character* iconName() const = 0;

    String nextAvailableNameForThisIfAddedTo(const MutableFolder&) const;

    count numberOfCratesAndTrackEntriesContainedWithin() const;
    boolean hasTracksNotOnVolume(const Volume&) const;
    boolean hasMovieTracks() const;

    virtual count numberOfSubCrates() const = 0;
    virtual Common::SubCrate subCrateAtIndex(count) const = 0;
    virtual Common::MutableSubCrate subCrateAtIndex(count) = 0;

    virtual count numberOfTracks() const = 0;
    virtual NotNull<const Track*> trackAtIndex(count) const = 0;
    virtual NotNull<MutableTrack*> trackAtIndex(count) = 0;
    virtual Array<NotNull<const Track*>> tracks() const = 0;
    virtual Array<NotNull<MutableTrack*>> tracks() = 0;

    void removeTrackWithRelativeFilePath(const FilePath&)
    {
        NXA_ALOG("This is unsupported.");
    }
    void removeTrackWithAbsoluteFilePath(const FilePath&)
    {
        NXA_ALOG("This is unsupported.");
    }
    void removeAllTracks()
    {
        NXA_ALOG("This is unsupported.");
    }

    virtual boolean canHavePredicates() const = 0;
    virtual Optional<TrackPredicate> maybePredicate() const = 0;
    virtual void setPredicate(const Optional<TrackPredicate>&) = 0;

    virtual boolean isOrganizedBy(const Common::Tag&) const = 0;
    virtual void organizeBy(Common::MutableTag&) = 0;
    virtual void removeOrganizationBy(Common::MutableTag&) = 0;

    virtual boolean isOrganizedBy(Common::Property::TypeID) const = 0;
    virtual void organizeBy(Common::Property::TypeID typeID) = 0;
    virtual void removeOrganizationBy(Common::Property::TypeID typeID) = 0;
};

} }
