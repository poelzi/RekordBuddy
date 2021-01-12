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

#include <CommonCollection/Crates/Folder.hpp>
#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Crates/SmartPlaylist.hpp>

using namespace NxA;
using namespace NxA::Common;

// -- Template Specializations
template <>
    Array<NotNull<const Track*>> Folder::tracksIn(const Playlist& playlist)
    {
        return Playlist::tracksIn<NotNull<const Track*>>(playlist);
    }
template <>
    Array<NotNull<MutableTrack*>> Folder::tracksIn(MutablePlaylist& playlist)
    {
        return Playlist::tracksIn<NotNull<MutableTrack*>>(static_cast<MutablePlaylist&>(playlist));
    }
template <>
    Array<NotNull<const Track*>> Folder::tracksIn(const SmartPlaylist& smartPlaylist)
    {
        return Playlist::tracksIn<NotNull<const Track*>>(smartPlaylist);
    }
template <>
    Array<NotNull<MutableTrack*>> Folder::tracksIn(MutableSmartPlaylist& smartPlaylist)
    {
        return Playlist::tracksIn<NotNull<MutableTrack*>>(static_cast<MutableSmartPlaylist&>(smartPlaylist));
    }

// -- Instance Methods

count Folder::numberOfCratesAndTrackEntriesContainedWithin() const
{
    // -- The crate always counts itself.
    count numberOfCratesAndTrackEntriesContainedWithin = 1;

    for (count index = 0; index < this->numberOfSubCrates(); ++index) {
        numberOfCratesAndTrackEntriesContainedWithin += this->subCrateAtIndex(index).apply([](auto& subCrate) {
            return subCrate->numberOfCratesAndTrackEntriesContainedWithin();
        });
    }

    return numberOfCratesAndTrackEntriesContainedWithin;
}

boolean Folder::hasTracksNotOnVolume(const Volume& volume) const
{
    return Folder::hasTracksNotOnVolume(*this, volume);
}

boolean Folder::hasMovieTracks() const
{
    return Folder::hasMovieTracks(*this);
}

Optional<count> MutableFolder::p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Playlist*> playlist,
                                                                            FindExisting findExisting)
{
    return this->p_maybeIndexOfExistingSubCrateWithSameNameAs<Common::MutablePlaylist>(playlist.get(), findExisting);
}

Optional<count> MutableFolder::p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const SmartPlaylist*> smartPlaylist,
                                                                            FindExisting findExisting)
{
    return this->p_maybeIndexOfExistingSubCrateWithSameNameAs<Common::MutablePlaylist>(smartPlaylist.get(), findExisting);
}

Optional<count> MutableFolder::p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<const Folder*> folder,
                                                                            FindExisting findExisting)
{
    return this->p_maybeIndexOfExistingSubCrateWithSameNameAs<Common::MutableFolder>(folder.get(), findExisting);
}

Optional<count> MutableFolder::p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<MutablePlaylist*> playlist,
                                                                            FindExisting findExisting)
{
    return this->p_maybeIndexOfExistingSubCrateWithSameNameAs<Common::MutablePlaylist>(playlist.get(), findExisting);
}

Optional<count> MutableFolder::p_maybeIndexOfExistingSubCrateWithSameNameAs(NotNull<MutableFolder*> folder,
                                                                            FindExisting findExisting)
{
    return this->p_maybeIndexOfExistingSubCrateWithSameNameAs<Common::MutableFolder>(folder.get(), findExisting);
}

void MutableFolder::p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                               NotNull<const Playlist*> playlist,
                                                                               const std::function<void(void)>& callback)
{
    auto newCrateIndex = this->numberOfSubCrates();
    this->newPlaylistWithName(playlist->name())->setWithSameTracksAsWithPerItemProgressCallBack(*playlist,
                                                                                                callback,
                                                                                                MutablePlaylist::AndUpdateTracks::No);
    this->p_moveSubCrateAtIndexToIndex(newCrateIndex, index);
}

void MutableFolder::p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                               NotNull<const SmartPlaylist*> smartPlaylist,
                                                                               const std::function<void(void)>& callback)
{
    auto newCrateIndex = this->numberOfSubCrates();
    this->newPlaylistWithName(smartPlaylist->name())->setWithSameTracksAsWithPerItemProgressCallBack(*smartPlaylist,
                                                                                                     callback,
                                                                                                     MutablePlaylist::AndUpdateTracks::No);
    this->p_moveSubCrateAtIndexToIndex(newCrateIndex, index);
}

void MutableFolder::p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                               NotNull<const Folder*> folder,
                                                                               const std::function<void(void)>& callback)
{
    auto newCrateIndex = this->numberOfSubCrates();
    this->newFolderWithName(folder->name())->setWithSameSubCratesAsWithPerItemProgressCallBack(*folder,
                                                                                               callback,
                                                                                               MutableFolder::AndUpdateTracks::No);
    this->p_moveSubCrateAtIndexToIndex(newCrateIndex, index);
}

void MutableFolder::p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                               NotNull<MutablePlaylist*> playlist,
                                                                               const std::function<void(void)>& callback)
{
    auto newCrateIndex = this->numberOfSubCrates();
    this->newPlaylistWithName(playlist->name())->setWithSameTracksAsWithPerItemProgressCallBack(*playlist,
                                                                                                callback,
                                                                                                MutablePlaylist::AndUpdateTracks::No);
    this->p_moveSubCrateAtIndexToIndex(newCrateIndex, index);
}

void MutableFolder::p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                               NotNull<MutableSmartPlaylist*> smartPlaylist,
                                                                               const std::function<void(void)>& callback)
{
    auto newCrateIndex = this->numberOfSubCrates();
    this->newPlaylistWithName(smartPlaylist->name())->setWithSameTracksAsWithPerItemProgressCallBack(*smartPlaylist,
                                                                                                     callback,
                                                                                                     MutablePlaylist::AndUpdateTracks::No);
    this->p_moveSubCrateAtIndexToIndex(newCrateIndex, index);
}

void MutableFolder::p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                               NotNull<MutableFolder*> folder,
                                                                               const std::function<void(void)>& callback)
{
    auto newCrateIndex = this->numberOfSubCrates();
    this->newFolderWithName(folder->name())->setWithSameSubCratesAsWithPerItemProgressCallBack(*folder,
                                                                                               callback,
                                                                                               MutableFolder::AndUpdateTracks::No);

    NXA_ASSERT_TRUE_WITH_BLOCK(index <= this->numberOfSubCrates(), [this, &index, &newCrateIndex]() {
        CrashLog::addUserInfoWithKey(String::stringWithFormat("%llu", index), "index");
        CrashLog::addUserInfoWithKey(String::stringWithFormat("%llu", newCrateIndex), "newCrateIndex");
        CrashLog::addUserInfoWithKey(String::stringWithFormat("%llu", this->numberOfSubCrates()), "numberOfSubCrates");
    });

    this->p_moveSubCrateAtIndexToIndex(newCrateIndex, index);
}

void MutableFolder::addSubCrateAtIndexAsCopyOfWithPerItemProgressCallBack(count index,
                                                                          SourceCrate subCrate,
                                                                          const std::function<void(void)>& callback,
                                                                          AndUpdateTracks andUpdateTracks)
{
    NXA_ASSERT_TRUE_WITH_BLOCK(index <= this->numberOfSubCrates(), [this, &index]() {
        CrashLog::addUserInfoWithKey(String::stringWithFormat("%llu", index), "index");
        CrashLog::addUserInfoWithKey(String::stringWithFormat("%llu", this->numberOfSubCrates()), "numberOfSubCrates");
    });

    MutableSet<NotNull<const Track*>> tracksToUpdate;

    auto collection = this->collection();
    auto thisMustHaveTracksOnSameVolume = collection->mustHaveTracksOnTheSameVolume();
    auto thisVolume = collection->volume();

    auto weAreAlwaysOnTheSameVolume = subCrate.apply([this,
                                                      collection,
                                                      thisVolume,
                                                      thisMustHaveTracksOnSameVolume,
                                                      &tracksToUpdate,
                                                      &index,
                                                      andUpdateTracks,
                                                      &callback](auto&& crateToAdd) {
        auto otherCollection = crateToAdd->collection();
        auto sourceIsInTheSameCollection = Common::MutableCollection::isEqual(*collection, *otherCollection);

        this->p_insertNewCrateAtIndexAsCopyOfWithPerItemProgressCallBack(index, crateToAdd, callback);

#if defined(NXA_DEBUG_PROGRESS_CALLBACK)
        NXA_DLOG_WITH_FORMAT("Updating Crate '%s'.\n", crateToAdd->path().asString().asUTF8());
#endif
        callback();

        if (!sourceIsInTheSameCollection && (andUpdateTracks == AndUpdateTracks::Yes)) {
            // -- We want to update each track only once so we gather them all here and update them later.
            // -- (if we are dealing with a crate on the same collection we don't need to update any tracks).
            const auto* constCrateToAdd = crateToAdd.get();
            tracksToUpdate.add(constCrateToAdd->tracks());
        }

        auto otherHasTracksOnSameVolume = otherCollection->mustHaveTracksOnTheSameVolume();
        return thisMustHaveTracksOnSameVolume && otherHasTracksOnSameVolume && (thisVolume == otherCollection->volume());
    });

    for (auto&& track : tracksToUpdate) {
        auto weAreOnTheSameVolume = weAreAlwaysOnTheSameVolume || (track->volume() == thisVolume);

        if (thisMustHaveTracksOnSameVolume && !weAreOnTheSameVolume) {
            // -- Other track is not on the same volume as this one so we skip it.
            continue;
        }

        MutableTrack* trackToUpdate;
        if (weAreOnTheSameVolume) {
            auto maybeNewRelativeFilePath = Track::maybeFilePathForTrackRelativeToVolume(track,
                                                                                         this->collection()->volume(),
                                                                                         weAreOnTheSameVolume ? Track::TrackIsOnSameVolume::Yes :
                                                                                         Track::TrackIsOnSameVolume::DontKnow);
            if (!maybeNewRelativeFilePath.isValid()) {
                // -- Something went wrong, we can't make a relative path for that collection that should accept tracks from all volumes.
                continue;
            }

            trackToUpdate = collection->trackWithRelativeFilePath(*maybeNewRelativeFilePath).get();
        }
        else {
            trackToUpdate = collection->trackWithAbsoluteFilePath(track->absoluteFilePath()).get();
        }

        trackToUpdate->setWithSamePropertiesAs(*track);

#if defined(NXA_DEBUG_PROGRESS_CALLBACK)
        NXA_DLOG_WITH_FORMAT("Updating Track '%s'.\n", trackToUpdate->absoluteFilePath().asEncodedString().asUTF8());
#endif
        callback();
    }
}

void MutableFolder::removeTrackWithRelativeFilePath(const FilePath& filePath)
{
    for (count index = 0; index < this->numberOfSubCrates(); ++index) {
        this->subCrateAtIndex(index).apply([&filePath](auto& subCrate) {
            subCrate->removeTrackWithRelativeFilePath(filePath);
        });
    }
}

void MutableFolder::removeTrackWithAbsoluteFilePath(const FilePath& filePath)
{
    for (count index = 0; index < this->numberOfSubCrates(); ++index) {
        this->subCrateAtIndex(index).apply([&filePath](auto& subCrate) {
            subCrate->removeTrackWithAbsoluteFilePath(filePath);
        });
    }
}

void MutableFolder::removeAllTracks()
{
    for (count index = 0; index < this->numberOfSubCrates(); ++index) {
        this->subCrateAtIndex(index).apply([](auto& subCrate) {
            subCrate->removeAllTracks();
        });
    }
}

Optional<NotNull<MutablePlaylist*>> MutableFolder::maybeExistingPlaylistWithName(const String& name)
{
    for (count index = 0; index < this->numberOfSubCrates(); ++index) {
        auto subCrate = this->subCrateAtIndex(index);
        auto maybeAsPlaylist = subCrate.maybeGet<NotNull<MutablePlaylist*>>();
        if (maybeAsPlaylist.isValid() && (*maybeAsPlaylist)->name() == name) {
            return *maybeAsPlaylist;
        }
    }

    return nothing;
}

NotNull<MutablePlaylist*> MutableFolder::newPlaylistWithNameAtIndex(const String& name, count index)
{
    auto numberOfSubCrates = this->numberOfSubCrates();
    NXA_ASSERT_TRUE(index <= numberOfSubCrates);

    auto newPlaylist = this->newPlaylistWithName(name);
    this->p_moveSubCrateAtIndexToIndex(numberOfSubCrates, index);
    return newPlaylist;
}

NotNull<MutableFolder*> MutableFolder::newFolderWithNameAtIndex(const String& name, count index)
{
    auto numberOfSubCrates = this->numberOfSubCrates();
    NXA_ASSERT_TRUE(index <= numberOfSubCrates);

    auto newFolder = this->newFolderWithName(name);
    this->p_moveSubCrateAtIndexToIndex(numberOfSubCrates, index);
    return newFolder;
}

NotNull<MutableSmartPlaylist*> MutableFolder::newSmartPlaylistWithNameAtIndex(const String& name, count index)
{
    auto numberOfSubCrates = this->numberOfSubCrates();
    NXA_ASSERT_TRUE(index <= numberOfSubCrates);

    auto newSmartPlaylist = this->newSmartPlaylistWithName(name);
    this->p_moveSubCrateAtIndexToIndex(numberOfSubCrates, index);
    return newSmartPlaylist;
}

String MutableFolder::nextAvailableNameForThisIfAddedTo(const MutableFolder& folder) const
{
    return folder.nextAvailableNameForFolderNamed(this->name());
}

count MutableFolder::numberOfCratesAndTrackEntriesContainedWithin() const
{
    // -- The crate always counts itself.
    count numberOfCratesAndTrackEntriesContainedWithin = 1;

    for (count index = 0; index < this->numberOfSubCrates(); ++index) {
        numberOfCratesAndTrackEntriesContainedWithin += this->subCrateAtIndex(index).apply([](auto& subCrate) {
            return subCrate->numberOfCratesAndTrackEntriesContainedWithin();
        });
    }

    return numberOfCratesAndTrackEntriesContainedWithin;
}

boolean MutableFolder::hasTracksNotOnVolume(const Volume& volume) const
{
    return Folder::hasTracksNotOnVolume(*this, volume);
}

boolean MutableFolder::hasMovieTracks() const
{
    return Folder::hasMovieTracks(*this);
}
