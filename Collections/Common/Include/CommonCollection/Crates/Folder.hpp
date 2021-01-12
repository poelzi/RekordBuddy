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
#include <CommonCollection/Crates/SmartPlaylist.hpp>
#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/CratePath.hpp>

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
class Track;
class MutableTrack;

// -- Public Interface
class Folder : public Uncopyable
{
public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const Folder& first, const T& second)
        {
            return (first.collection() == second.collection()) && (Folder::cratePathFor(first) == Folder::cratePathFor(second));
        }
    template <class T>
        inline static boolean isLessThan(const Folder& first, const T& second)
        {
            return (first.collection() < second.collection()) && (Folder::cratePathFor(first) < Folder::cratePathFor(second));
        }

    template <class T>
        static CratePath cratePathFor(const T& folder)
        {
            auto name = folder.name();
            NXA_ASSERT_FALSE(name.isEmpty());

            auto maybeParent = folder.maybeParentFolder();
            if (maybeParent.isValid()) {
                return Folder::cratePathFor(**maybeParent).pathForChildNamed(name);
            }

            return CratePath::forCrateNamed(name);
        }

    template <class TrackPointerType, class T>
        static Array<TrackPointerType> tracksIn(T& folder)
        {
            MutableSet<TrackPointerType> results;

            for (count crateIndex = 0; crateIndex < folder.numberOfSubCrates(); ++crateIndex) {
                folder.subCrateAtIndex(crateIndex).apply([&results](auto&& subCrate) {
                    results.add(Folder::tracksIn<TrackPointerType>(*subCrate));
                });
            }

            return { std::move(results) };
        }

    template <class T>
        static Array<count> indicesOfSubCrateWithNameIn(const String& name, T& folder)
        {
            MutableArray<count> results;

            for (count index = 0; index < folder.numberOfSubCrates(); ++index) {
                if (name == folder.subCrateAtIndex(index).apply([](auto& crate) {
                        return crate->name();
                    })) {
                    results.append(index);
                }
            }

            return std::move(results);
        }

    template <class T>
        static boolean hasTracksNotOnVolume(const T& folder, const Volume& volume)
        {
            for (count index = 0; index < folder.numberOfSubCrates(); ++index) {
                if (folder.subCrateAtIndex(index).apply([volume](auto& subCrate) {
                    return subCrate->hasTracksNotOnVolume(volume);
                })) {
                    return true;
                }
            }

            return false;
        }
    template <class T>
        static boolean hasMovieTracks(const T& folder)
        {
            for (count index = 0; index < folder.numberOfSubCrates(); ++index) {
                if (folder.subCrateAtIndex(index).apply([](auto& subCrate) {
                    return subCrate->hasMovieTracks();
                })) {
                    return true;
                }
            }

            return false;
        }

    // -- Constructors & Destructors
    virtual ~Folder() = default;

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual Optional<NotNull<const Folder*>> maybeParentFolder() const = 0;

    virtual Time lastModificationTime() const = 0;

    virtual String name() const = 0;
    virtual CratePath path() const = 0;

    count numberOfCratesAndTrackEntriesContainedWithin() const;
    boolean hasTracksNotOnVolume(const Volume&) const;
    boolean hasMovieTracks() const;

    virtual Array<NotNull<const Track*>> tracks() const = 0;

    virtual count numberOfSubCrates() const = 0;
    virtual SubCrate subCrateAtIndex(count) const = 0;
};

class MutableFolder : public Uncopyable
{
public:
    // -- Constants
    enum class AndUpdateTracks {
        No,
        Yes
    };
    enum class FindExisting {
        EvenIfDifferentType,
        OnlyIfTheSameType,
    };

    // -- Types
    using SourceCrate = Variant<NotNull<const Playlist*>, NotNull<const SmartPlaylist*>, NotNull<const Folder*>,
                                NotNull<MutablePlaylist*>, NotNull<MutableSmartPlaylist*>, NotNull<MutableFolder*>>;
    using MutableSourceCrate = Variant<NotNull<MutablePlaylist*>, NotNull<MutableFolder*>>;

private:
    // -- Private Instance Methods
    template <class T, class SourceT>
        Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(const SourceT* crate, FindExisting findExisting)
        {
            const String& name = crate->name();
            auto existingIndices = Common::Folder::indicesOfSubCrateWithNameIn(name, *this);
            if (!existingIndices.length()) {
                return nothing;
            }

            // -- We have at least one crate with the same name that already exists in this folder.
            Optional<NotNull<T*>> maybeExistingCrate;
            Optional<count> maybeIndexFound;

            for (auto&& subCrateIndex : existingIndices) {
                maybeExistingCrate = maybeGet<NotNull<T*>>(this->subCrateAtIndex(subCrateIndex));
                if (maybeExistingCrate.isValid()) {
                    maybeIndexFound = subCrateIndex;
                    break;
                }
            }

            // -- If we have more than one subcrate with the name then one of the must match the type we are looking for.
            NXA_ASSERT_TRUE((existingIndices.length() < 2) || maybeExistingCrate.isValid());

            if (maybeExistingCrate.isValid()) {
                // -- The existing crate is the type we're looking for.
                return maybeIndexFound;
            }
            else {
                // -- The existing crate is a different type.
                if (findExisting != FindExisting::OnlyIfTheSameType) {
                    return existingIndices.firstObject();
                }
            }

            return nothing;
        }
    Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Playlist*>, FindExisting);
    Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const SmartPlaylist*>, FindExisting);
    Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Folder*>, FindExisting);
    Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<MutablePlaylist*>, FindExisting);
    Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<MutableFolder*>, FindExisting);

    void p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count, NotNull<const Playlist*>, const std::function<void(void)>&);
    void p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count, NotNull<const SmartPlaylist*>, const std::function<void(void)>&);
    void p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count, NotNull<const Folder*>, const std::function<void(void)>&);
    void p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count, NotNull<MutablePlaylist*>, const std::function<void(void)>&);
    void p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count, NotNull<MutableSmartPlaylist*>, const std::function<void(void)>&);
    void p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count, NotNull<MutableFolder*>, const std::function<void(void)>&);

    virtual String p_nextAvailableNameForSubCrateNamed(const String& name, boolean isAFolder) const
    {
        count newEntryIndex = 1;

        while (1) {
            auto newEntryName = (newEntryIndex > 1) ? String::stringWithFormat("%s %llu", name.asUTF8(), newEntryIndex) : name;
            boolean alreadyThere = false;

            for (count crateIndex = 0; crateIndex < this->numberOfSubCrates(); ++crateIndex) {
                alreadyThere = this->subCrateAtIndex(crateIndex).apply([&newEntryName](auto&& subCrate) {
                    return (subCrate->name() == newEntryName);
                });

                if (alreadyThere) {
                    break;
                }
            }

            if (!alreadyThere) {
                return newEntryName;
            }

            ++newEntryIndex;
        }
    }

protected:
    // -- Protected Instance Methods
    virtual void p_moveSubCrateAtIndexToIndex(count, count) = 0;

    Optional<count> p_maybeIndexOfExistingSubCrateWithSameNameAs(SourceCrate subCrate, FindExisting findExisting)
    {
        return subCrate.apply([this, findExisting](auto&& crate) {
            return this->p_maybeIndexOfExistingSubCrateWithSameNameAs(crate, findExisting);
        });
    }

    void p_moveSubCrateToIndex(NotNull<const Playlist*>, count)
    {
        NXA_ALOG("Can't move this.");
    }
    void p_moveSubCrateToIndex(NotNull<const SmartPlaylist*>, count)
    {
        NXA_ALOG("Can't move this.");
    }
    void p_moveSubCrateToIndex(NotNull<const Folder*>, count)
    {
        NXA_ALOG("Can't move this.");
    }
    virtual void p_moveSubCrateToIndex(NotNull<MutablePlaylist*>, count) = 0;
    virtual void p_moveSubCrateToIndex(NotNull<MutableFolder*>, count) = 0;

public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const MutableFolder& first, const T& second)
        {
            return (first.collection() == second.collection()) && (Folder::cratePathFor(first) == Folder::cratePathFor(second));
        }
    template <class T>
        inline static boolean isLessThan(const MutableFolder& first, const T& second) noexcept
        {
            return (first.collection() < second.collection()) && (Folder::cratePathFor(first) < Folder::cratePathFor(second));
        }
    template <class T>
        static void removeFromParent(T& crateToRemove)
        {
            auto maybeParent = crateToRemove->maybeParentFolder();
            NXA_ASSERT_TRUE(maybeParent.isValid());

            auto& parent = *maybeParent;

            auto crateToRemoveAsSubCrate = MutableSubCrate{ crateToRemove };

            for (count index = 0; index < (*maybeParent)->numberOfSubCrates(); ++index) {
                if (parent->subCrateAtIndex(index) == crateToRemoveAsSubCrate) {
                    parent->removeSubCrateAtIndex(index);
                    return;
                }
            }

            NXA_ALOG("Couldn't find crate in parent.");
        }
    template <>
        void removeFromParent(NotNull<const Playlist *>&)
        {
            NXA_ALOG("Can't remove a non-mutable type.");
        }
    template <>
        void removeFromParent(NotNull<const SmartPlaylist *>&)
        {
            NXA_ALOG("Can't remove a non-mutable type.");
        }
    template <>
        void removeFromParent(NotNull<const Folder *>&)
        {
            NXA_ALOG("Can't remove a non-mutable type.");
        }

    // -- Constructors & Destructors
    virtual ~MutableFolder() = default;

    // -- Operators
    inline bool operator==(const MutableFolder& other) const noexcept
    {
        return (this->collection() == other.collection()) && (Folder::cratePathFor(*this) == Folder::cratePathFor(other));
    }
    inline bool operator!=(const MutableFolder& other) const noexcept
    {
        return !this->operator==(other);
    }
    inline bool operator<(const MutableFolder& other) const noexcept
    {
        return (this->collection() < other.collection()) && (Folder::cratePathFor(*this) < Folder::cratePathFor(other));
    }

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual Optional<NotNull<const Folder*>> maybeParentFolder() const = 0;
    virtual Optional<NotNull<MutableFolder*>> maybeParentFolder() = 0;

    virtual Time lastModificationTime() const = 0;

    virtual boolean canBeCloned() const
    {
        return false;
    }
    virtual NotNull<MutableFolder*> cloneWithName(const String&)
    {
        NXA_ALOG("Unsupported method call.");
    }

    virtual String name() const = 0;
    virtual void setName(const String&) = 0;
    virtual CratePath path() const = 0;

    String nextAvailableNameForThisIfAddedTo(const MutableFolder&) const;

    count numberOfCratesAndTrackEntriesContainedWithin() const;
    boolean hasTracksNotOnVolume(const Volume&) const;
    boolean hasMovieTracks() const;

    virtual Array<NotNull<const Track*>> tracks() const = 0;
    virtual Array<NotNull<MutableTrack*>> tracks() = 0;

    virtual count numberOfSubCrates() const = 0;
    virtual SubCrate subCrateAtIndex(count) const = 0;
    virtual MutableSubCrate subCrateAtIndex(count) = 0;
    void addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count,
                                                               SourceCrate,
                                                               const std::function<void(void)>&,
                                                               AndUpdateTracks = AndUpdateTracks::Yes);
    virtual Optional<count> maybeIndexOfExistingSubCrateWithSameNameAs(SourceCrate subCrate)
    {
        return this->p_maybeIndexOfExistingSubCrateWithSameNameAs(subCrate, FindExisting::EvenIfDifferentType);
    }
    void moveSubCrateToIndex(MutableSourceCrate subCrate, count index)
    {
        subCrate.apply([this, index](auto&& subCrate) {
            // -- We have to be on the same collection for this method to be used.
            NXA_ASSERT_TRUE(Collection::isEqual(*const_cast<const MutableFolder*>(this)->collection(), *subCrate->collection()));

            auto maybeParentFolder = subCrate->maybeParentFolder();
            NXA_ASSERT_TRUE(maybeParentFolder.isValid());

            if (maybeParentFolder->get() == this) {
                // -- We are moving this crate within the same folder.
                void* subCratePointer = subCrate.get();

                count currentIndex = 0;
                for (;currentIndex < this->numberOfSubCrates(); ++currentIndex) {
                    if (this->subCrateAtIndex(currentIndex).apply([](auto& subCrate) -> void* { return subCrate.get(); }) == subCratePointer) {
                        break;
                    }
                }

                NXA_ASSERT_TRUE(currentIndex < this->numberOfSubCrates());

                if (currentIndex == index) {
                    // -- We are moving this crate to its existing position so we do nothing.
                    return;
                }

                this->p_moveSubCrateAtIndexToIndex(currentIndex, index);
            }
            else {
                // -- We are moving this crate from another folder to this one.
                this->p_moveSubCrateToIndex(subCrate, index);
            }
        });
    }
    virtual void removeSubCrateAtIndex(count) = 0;
    virtual void removeAllSubCrates()
    {
        count numberOfCratesLeft = this->numberOfSubCrates();
        while (numberOfCratesLeft) {
            this->removeSubCrateAtIndex(0);
            --numberOfCratesLeft;
        }
    }

    void removeTrackWithRelativeFilePath(const FilePath&);
    void removeTrackWithAbsoluteFilePath(const FilePath&);
    void removeAllTracks();

    Optional<NotNull<MutablePlaylist*>> maybeExistingPlaylistWithName(const String&);
    virtual NotNull<MutablePlaylist*> newPlaylistWithName(const String&) = 0;
    virtual NotNull<MutableFolder*> newFolderWithName(const String&) = 0;
    NotNull<MutablePlaylist*> newPlaylistWithNameAtIndex(const String& name, count index);
    NotNull<MutableFolder*> newFolderWithNameAtIndex(const String& name, count index);

    virtual boolean supportsSmartPlaylists()
    {
        return false;
    }
    virtual NotNull<MutableSmartPlaylist*> newSmartPlaylistWithName(const String&)
    {
        NXA_ALOG("Unsupported method call.");
    }
    NotNull<MutableSmartPlaylist*> newSmartPlaylistWithNameAtIndex(const String& name, count index);

    virtual boolean canReceive(SourceCrate crate) const
    {
        auto maybeAsFolder = crate.maybeGet<NotNull<MutableFolder*>>();
        if (maybeAsFolder.isValid() && (maybeAsFolder->get() == this)) {
            // -- We can't receive ourselves;
            return false;
        }

        return true;
    }

    inline String nextAvailableNameForFolderNamed(const String& name) const
    {
        return this->p_nextAvailableNameForSubCrateNamed(name, true);
    }
    inline String nextAvailableNameForPlaylistNamed(const String& name) const
    {
        return this->p_nextAvailableNameForSubCrateNamed(name, false);
    }

    template <class T>
        void setWithSameSubCratesAsWithPerItemProgressCallBack(const T& other,
                                                               const std::function<void(void)>& callback,
                                                               AndUpdateTracks andUpdateTracks)
        {
            auto collection = this->collection();
            auto otherCollection = other.collection();
            auto sourceIsInTheSameCollection = Common::MutableCollection::isEqual(*collection, *otherCollection);

            // -- If we are dealing with a crate on the same collection we don't need to update any tracks.
            if (!sourceIsInTheSameCollection && (andUpdateTracks == AndUpdateTracks::Yes)) {
                auto thisMustHaveTracksOnSameVolume = collection->mustHaveTracksOnTheSameVolume();
                auto thisVolume = collection->volume();
                auto otherHasTracksOnSameVolume = otherCollection->mustHaveTracksOnTheSameVolume();

                auto weAreAlwaysOnTheSameVolume = thisMustHaveTracksOnSameVolume && otherHasTracksOnSameVolume && (thisVolume == otherCollection->volume());

                for (auto&& track : Folder::tracksIn<NotNull<const Track*>>(other)) {
                    auto weAreOnTheSameVolume = weAreAlwaysOnTheSameVolume || (track->volume() == thisVolume);

                    if (thisMustHaveTracksOnSameVolume && !weAreOnTheSameVolume) {
                        // -- Other track is not on the same volume as this one so we skip it.
                        continue;
                    }

                    MutableTrack* trackToUpdate;
                    if (weAreOnTheSameVolume) {
                        auto maybeNewRelativeFilePath = Track::maybeFilePathForTrackRelativeToVolume(track,
                                                                                                     this->collection()->volume(),
                                                                                                     weAreOnTheSameVolume ? Track::TrackIsOnSameVolume::Yes : Track::TrackIsOnSameVolume::DontKnow);
                        if (!maybeNewRelativeFilePath.isValid()) {
                            // -- Something went wrong, we can't make a relative path for that collection that should accept tracks from all volumes.
                            continue;
                        }

                        trackToUpdate = this->collection()->trackWithRelativeFilePath(*maybeNewRelativeFilePath).get();
                    }
                    else {
                        trackToUpdate = this->collection()->trackWithAbsoluteFilePath(track->absoluteFilePath()).get();
                    }

                    trackToUpdate->setWithSamePropertiesAs(*track);

                    callback();
                }
            }

            this->removeAllSubCrates();

            for (count crateIndex = 0; crateIndex < other.numberOfSubCrates(); ++crateIndex) {
                other.subCrateAtIndex(crateIndex).apply([this, crateIndex, &callback](auto& subCrateToAdd) {
                    this->addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(crateIndex,
                                                                                subCrateToAdd,
                                                                                callback,
                                                                                AndUpdateTracks::No);
                });
            }
        }
};

// -- Folder Template Specializations
template <>
    Array<NotNull<const Track*>> Folder::tracksIn(const Playlist&);
template <>
    Array<NotNull<MutableTrack*>> Folder::tracksIn(MutablePlaylist&);
template <>
    Array<NotNull<const Track*>> Folder::tracksIn(const SmartPlaylist&);
template <>
    Array<NotNull<MutableTrack*>> Folder::tracksIn(MutableSmartPlaylist&);

} }
