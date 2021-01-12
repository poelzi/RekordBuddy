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

#include <CommonCollection/Tracks/Property.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class Artist;
class MutableArtist;
class Tag;
class MutableTag;
class MusicalKey;
class MutableMusicalKey;
class Track;
class MutableTrack;
class Folder;
class MutableFolder;
class Playlist;
class MutablePlaylist;

// -- Public Interface
class Collection
{
    // -- Friends
    friend class MutableCollection;

    // -- Private Instance Methods
    virtual String p_description() const = 0;

protected:
    // -- Protected Class Methods
    template <class T>
        inline static Optional<NotNull<const Common::Track*>> p_maybeExistingTrackWithRelativeFilePathIn(const FilePath& relativeFilePath, const T& collection)
        {
            // -- If the collection does not accepts tracks on other volumes then it must override this method.
            NXA_ASSERT_FALSE(collection.mustHaveTracksOnTheSameVolume());

            return collection.maybeExistingTrackWithAbsoluteFilePath(FilePath::filePathByJoiningPaths(collection.volume(), relativeFilePath));
        }
    template <class T>
        inline static Optional<NotNull<const Track*>> p_maybeExistingTrackWithAbsoluteFilePathIn(const FilePath& absoluteFilePath, const T& collection)
        {
            // -- If the collection accepts tracks on other volumes then it must override this method.
            NXA_ASSERT_TRUE(collection.mustHaveTracksOnTheSameVolume());

            auto volume = Volume{ absoluteFilePath };
            if (volume == collection.volume()) {
                return collection.maybeExistingTrackWithRelativeFilePath(*absoluteFilePath.maybeRelativeToVolume(volume));
            }

            return nothing;
        }

public:
    // -- Constants
    enum class Type {
        RekordBuddy,
        rekordbox,
        Serato,
        Traktor,
        Engine,
        VirtualDJ,
        PCDJ,
        iTunes,
        MixedInKey,
        TrackFile,
        AppleMusic,

        // -- This is required for static assertions.
        LastFlag
    };

    enum class Error {
        NotFound,
        CannotOpen,
        Corrupted,
        InvalidLicense,
        NeedsUpgrading
    };

    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const Collection& first, const T& second)
        {
            return (first.type() == second.type()) && (first.volume() == second.volume());
        }
    template <class T>
        inline static boolean isLessThan(const Collection& first, const T& second)
        {
            return (first.type() < second.type()) && (first.volume() < second.volume());
        }
    static String nameForType(Type type)
    {
        switch (type) {
            case Type::RekordBuddy: {
                return "Rekord Buddy"_String;
            }
            case Type::rekordbox: {
                return "rekordbox"_String;
            }
            case Type::Serato: {
                return "Serato"_String;
            }
            case Type::Traktor: {
                return "Traktor"_String;
            }
            case Type::Engine: {
                return "Engine"_String;
            }
            case Type::VirtualDJ: {
                return "VirtualDJ"_String;
            }
            case Type::PCDJ: {
                return "PCDJ"_String;
            }
            case Type::iTunes: {
                return "iTunes"_String;
            }
            case Type::MixedInKey: {
                return "Mixed In Key"_String;
            }
            case Type::TrackFile: {
                return "TrackFile"_String;
            }
            case Type::AppleMusic: {
                return "Apple Music"_String;
            }
            default: {
                return "<Unknown>"_String;
            }
        }
    }
    static void makeSureStringsFromPreferenceKeyAreNotAlreadyInThenAddOnesThatAreNot(UserPreferences&, const String&, MutableArray<String>&);

    // -- Constructors & Destructors
    virtual ~Collection() = default;

    // -- Instance Methods
    virtual boolean shouldBeOpenedLazily() const = 0;
    virtual boolean mustHaveTracksOnTheSameVolume() const = 0;
    virtual boolean hasTracksNotOnVolume(const Volume&) const;
    virtual boolean allowsMovieTracksInPlaylists() const = 0;

    virtual boolean providesCuePointsAndBeatGrid() const
    {
        return true;
    }

    virtual Optional<Collection::Error> open() = 0;
    virtual Optional<Collection::Error> lastOpenResult() const = 0;
    virtual String name() const = 0;
    virtual String fullName() const
    {
        auto collectionVolume = this->volume();
        if (collectionVolume == Volume::musicFolderVolume()) {
            return this->name();
        }
        else {
            return String::stringWithFormat("%s (on %s)", this->name().asUTF8(), collectionVolume.name().asUTF8());
        }
    }
    String description() const
    {
        auto result = MutableString{ this->p_description() };
        result.append(" (Read-Only)"_String);

        return { result };
    }
    virtual const character* iconName() const = 0;
    virtual Collection::Type type() const = 0;
    virtual Volume volume() const = 0;
    virtual Time lastModificationTime() const = 0;

    virtual boolean isOpened() const = 0;
    virtual Optional<String> maybeOpeningErrorDescription() const = 0;

    virtual void notifyUserPreferencesHaveChanged()
    {
        // -- This method can be overloaded for collections to grab any new values changed in the user preferences.
    }
    virtual const String& artistsSeparator() const = 0;
    virtual const String& genresSeparator() const = 0;
    virtual const String& musicalKeysSeparator() const = 0;

    virtual NotNull<const Folder*> rootFolder() const = 0;
    virtual NotNull<const Playlist*> tracks() const = 0;
    virtual Array<Unique<Artist>> artists() const = 0;
    virtual Array<Unique<MusicalKey>> musicalKeys() const = 0;
    virtual Array<Unique<Tag>> tags() const = 0;
    virtual Array<Property::TypeID> propertyTypes() const = 0;

    virtual Optional<NotNull<const Track*>> maybeExistingTrackWithRelativeFilePath(const FilePath&) const = 0;
    virtual Optional<NotNull<const Track*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&) const = 0;
};

class MutableCollection
{
    // -- Private Instance Methods
    virtual String p_description() const = 0;

public:
    // -- Class Methods
    template <class T>
        inline static boolean isEqual(const MutableCollection& first, const T& second)
        {
            return (first.type() == second.type()) && (first.volume() == second.volume());
        }
    template <class T>
        inline static boolean isLessThan(const MutableCollection& first, const T& second)
        {
            return (first.type() < second.type()) && (first.volume() < second.volume());
        }

    // -- Constructors & Destructors
    ~MutableCollection() = default;

    // -- Operators
    bool operator==(const MutableCollection& other) const noexcept
    {
        return MutableCollection::isEqual(*this, other);
    }
    bool operator!=(const MutableCollection& other) const noexcept
    {
        return !this->operator==(other);
    }

    // -- Instance Methods
    virtual boolean shouldBeOpenedLazily() const = 0;
    virtual boolean mustHaveTracksOnTheSameVolume() const = 0;
    virtual boolean hasTracksNotOnVolume(const Volume&) const;
    virtual boolean allowsMovieTracksInPlaylists() const = 0;

    virtual boolean providesCuePointsAndBeatGrid() const
    {
        return true;
    }
    virtual boolean canReceiveFiles() const
    {
        return false;
    }
    virtual boolean canReceiveFileAt(const FilePath&, Optional<String>&) const
    {
        return false;
    }
    virtual boolean receiveFileAt(const FilePath&)
    {
        NXA_ALOG("Unsupported method call.");
    }

    virtual Optional<Collection::Error> open() = 0;
    virtual Optional<Collection::Error> lastOpenResult() const = 0;
    virtual String name() const = 0;
    virtual String fullName() const
    {
        auto collectionVolume = this->volume();
        if (collectionVolume == Volume::musicFolderVolume()) {
            return this->name();
        }
        else {
            return String::stringWithFormat("%s (on %s)", this->name().asUTF8(), collectionVolume.name().asUTF8());
        }
    }
    String description() const
    {
        return this->p_description();
    }
    virtual const character* iconName() const = 0;
    virtual Collection::Type type() const = 0;
    virtual Volume volume() const = 0;
    virtual Time lastModificationTime() const = 0;

    virtual boolean isOpened() const = 0;
    virtual Optional<String> maybeOpeningErrorDescription() const = 0;
    virtual boolean hasChangesToSave() const = 0;

    virtual void markAsModifiedNow() = 0;
    virtual void reset() = 0;
    virtual void save() = 0;
    virtual void saveWithProgress(std::function<void(double)>&&) = 0;

    virtual void notifyUserPreferencesHaveChanged()
    {
        // -- This method can be overloaded for collections to grab any new values changed in the user preferences.
    }
    virtual const String& artistsSeparator() const = 0;
    virtual const String& genresSeparator() const = 0;
    virtual const String& musicalKeysSeparator() const = 0;

    virtual NotNull<const Folder*> rootFolder() const = 0;
    virtual NotNull<MutableFolder*> rootFolder() = 0;
    virtual NotNull<const Playlist*> tracks() const = 0;
    virtual NotNull<MutablePlaylist*> tracks() = 0;
    virtual Array<Unique<Artist>> artists() const = 0;
    virtual Array<Unique<MutableArtist>> artists() = 0;
    virtual Array<Unique<MusicalKey>> musicalKeys() const = 0;
    virtual Array<Unique<MutableMusicalKey>> musicalKeys() = 0;
    virtual Array<Unique<Tag>> tags() const = 0;
    virtual Array<Unique<MutableTag>> tags() = 0;
    virtual Array<Property::TypeID> propertyTypes() const = 0;

    virtual NotNull<MutableTrack*> trackWithRelativeFilePath(const FilePath&);
    virtual Optional<NotNull<const Track*>> maybeExistingTrackWithRelativeFilePath(const FilePath&) const = 0;
    virtual Optional<NotNull<MutableTrack*>> maybeExistingTrackWithRelativeFilePath(const FilePath&);

    virtual NotNull<MutableTrack*> trackWithAbsoluteFilePath(const FilePath&);
    virtual Optional<NotNull<const Track*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&) const = 0;
    virtual Optional<NotNull<MutableTrack*>> maybeExistingTrackWithAbsoluteFilePath(const FilePath&);
};

// -- Types.

using CollectionOfSomeSort = Variant<Shared<Collection>, Shared<MutableCollection>>;

} }
