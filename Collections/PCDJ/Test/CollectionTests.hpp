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

#include <PCDJCollection/Collection.hpp>

#include <Base/Test.hpp>

namespace NxA { namespace PCDJ {

// -- Forward Declarations
class MockMutableCollection;
using StrictMockMutableCollection = testing::StrictMock<MockMutableCollection>;

// -- This class mocks a PCDJ MutableCollection
class MockMutableCollection : public MutableCollection
{
    // -- Friends
    friend StrictMockMutableCollection;

    // -- Private Instance Variables
    count p_nextTrackID = 23;

    String p_artistsSeparator = ", "_String;
    String p_genresSeparator = ", "_String;
    String p_musicalKeysSeparator = ", "_String;

    // -- Private Constructors & Destructors
    MockMutableCollection(const FilePath& xmlFilePath, Volume volume, String name) : MutableCollection(xmlFilePath)
    {
        this->setTestLastAllTracksModificationTime("1981-10-22 02:16:40"_String);
        this->setTestLastPlaylistsModificationTime("1981-10-22 02:16:40"_String);

        NXA_DEFAULT_RETURN_ON_CALL(*this, type(), Common::Collection::Type::Traktor);
        NXA_DEFAULT_RETURN_ON_CALL(*this, volume(), volume);
        NXA_DEFAULT_RETURN_ON_CALL(*this, name(), name);
        NXA_DEFAULT_RETURN_ON_CALL(*this, isOpened(), true);
        NXA_DEFAULT_RETURN_REF_ON_CALL(*this, artistsSeparator(), p_artistsSeparator);
        NXA_DEFAULT_RETURN_REF_ON_CALL(*this, genresSeparator(), p_genresSeparator);
        NXA_DEFAULT_RETURN_REF_ON_CALL(*this, musicalKeysSeparator(), p_musicalKeysSeparator);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maximumNumberOfHotCuesToExport(), 8);
        NXA_DEFAULT_RETURN_ON_CALL(*this, maximumNumberOfGridMarkersToImport(), 4096);
        NXA_DEFAULT_RETURN_ON_CALL(*this, shouldExportHotCuesAlsoAsMemoryCues(), false);
        NXA_DEFAULT_RETURN_ON_CALL(*this, mustHaveTracksOnTheSameVolume(), false);
    }

public:
    // -- Factory Methods
#if defined(NXA_PLATFORM_WINDOWS)
    static Unique<StrictMockMutableCollection> strickMockMutableCollectionWith(const FilePath& xmlFilePath,
                                                                               String name = "C:"_String,
                                                                               Volume volume = Volume{ FilePath{ "C:\\" } })
#elif defined(NXA_PLATFORM_MACOS)
    static Unique<StrictMockMutableCollection> strickMockMutableCollectionWith(const FilePath& xmlFilePath,
                                                                               String name = "Macintosh HD"_String,
                                                                               Volume volume = Volume{ FilePath{ "/" } })
#else
    #error Unsupported platform.
#endif
    {
        return Unique<StrictMockMutableCollection>::with(xmlFilePath, volume, name);
    }

    // -- Instance Methods
    count nextTestTrackID()
    {
        return this->p_nextTrackID++;
    }
    void setTestLastPlaylistsModificationTime(String timeAsString)
    {
        auto maybeModificationTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);
        NXA_ASSERT_TRUE(maybeModificationTime.isValid());
        NXA_DEFAULT_RETURN_ON_CALL(*this, lastPlaylistsModificationTime(), *maybeModificationTime);
    }
    void setTestLastAllTracksModificationTime(String timeAsString)
    {
        auto maybeModificationTime = Time::maybeTimeFromStringInLocalTimeZoneUsingFormat(timeAsString, Time::defaultStringFormat);
        NXA_ASSERT_TRUE(maybeModificationTime.isValid());
        NXA_DEFAULT_RETURN_ON_CALL(*this, lastAllTracksModificationTime(), *maybeModificationTime);
    }
    void setTestArtistsSeparator(String separator)
    {
        this->p_artistsSeparator = separator;
    }
    void setTestGenresSeparator(String separator)
    {
        this->p_genresSeparator = separator;
    }
    void setTestMusicalKeysSeparator(String separator)
    {
        this->p_musicalKeysSeparator = separator;
    }
    void setTestMaximumNumberOfHotCuesToExport(count maximumNumberOfHotCues)
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, maximumNumberOfHotCuesToExport(), maximumNumberOfHotCues);
    }
    void setTestMaximumNumberOfGridMarkersToImport(count maximumNumberOfGridMarkersToImport)
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, maximumNumberOfGridMarkersToImport(), maximumNumberOfGridMarkersToImport);
    }
    void setTestExportHotCuesAlsoAsMemoryCues(boolean shouldExportHotCuesAlsoAsMemoryCues)
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, shouldExportHotCuesAlsoAsMemoryCues(), shouldExportHotCuesAlsoAsMemoryCues);
    }

    // -- Mocked Methods
    MOCK_CONST_METHOD0(shouldBeOpenedLazily, boolean());
    MOCK_CONST_METHOD0(mustHaveTracksOnTheSameVolume, boolean());
    MOCK_CONST_METHOD0(allowsMovieTracksInPlaylists, boolean());

    MOCK_CONST_METHOD1(maybeExistingTrackForTrackID, Optional<NotNull<const Common::Track*>>(count));
    MOCK_METHOD1(maybeExistingTrackForTrackID, Optional<NotNull<Common::MutableTrack*>>(count));

    MOCK_CONST_METHOD0(lastAllTracksModificationTime, Time());
    MOCK_CONST_METHOD0(lastPlaylistsModificationTime, Time());
    MOCK_CONST_METHOD0(markAllTracksAsModifiedNow, void());
    MOCK_CONST_METHOD0(markPlaylistsAsModifiedNow, void());

    MOCK_CONST_METHOD0(numberOfTracks, count());

    MOCK_CONST_METHOD1(trackAtIndex, NotNull<const Common::Track*>(count));
    MOCK_METHOD1(trackAtIndex, NotNull<Common::MutableTrack*>(count));
    MOCK_METHOD2(addTrackAtIndex, void(NotNull<Common::MutableTrack*>, count));

    MOCK_METHOD0(open, Optional<Common::Collection::Error>());

    MOCK_METHOD0(markAsModifiedNow, void());
    MOCK_METHOD0(reset, void());
    MOCK_METHOD0(save, void());
    // -- TODO: The argument type doesn't seem to work for mocking right now
    // -- MOCK_METHOD1(saveWithProgress, void(std::function<void(double)>&&));

    MOCK_METHOD0(notifyUserPreferencesHaveChanged, void());

    MOCK_METHOD0(rootFolder, NotNull<Common::MutableFolder*>());
    MOCK_METHOD0(tracks, NotNull<Common::MutablePlaylist*>());
    MOCK_METHOD0(artists, Array<Unique<Common::MutableArtist>>());
    MOCK_METHOD0(musicalKeys, Array<Unique<Common::MutableMusicalKey>>());
    MOCK_METHOD0(tags, Array<Unique<Common::MutableTag>>());

    MOCK_METHOD1(trackWithAbsoluteFilePath, NotNull<Common::MutableTrack*>(const FilePath&));
    MOCK_METHOD1(maybeExistingTrackWithAbsoluteFilePath, Optional<NotNull<Common::MutableTrack*>>(const FilePath&));

    MOCK_CONST_METHOD0(name, String());
    MOCK_CONST_METHOD0(description, Optional<String>());
    MOCK_CONST_METHOD0(type, Common::Collection::Type());
    MOCK_CONST_METHOD0(volume, Volume());
    MOCK_CONST_METHOD0(lastModificationTime, Time());

    MOCK_CONST_METHOD0(isOpened, boolean());
    MOCK_CONST_METHOD0(maybeOpeningErrorDescription, Optional<String>());
    MOCK_CONST_METHOD0(hasChangesToSave, boolean());

    MOCK_CONST_METHOD0(artistsSeparator, const String&());
    MOCK_CONST_METHOD0(genresSeparator, const String&());
    MOCK_CONST_METHOD0(musicalKeysSeparator, const String&());
    MOCK_CONST_METHOD0(maximumNumberOfHotCuesToExport, count());
    MOCK_CONST_METHOD0(maximumNumberOfGridMarkersToImport, count());
    MOCK_CONST_METHOD0(shouldExportHotCuesAlsoAsMemoryCues, boolean());

    MOCK_CONST_METHOD0(rootFolder, NotNull<const Common::Folder*>());
    MOCK_CONST_METHOD0(tracks, NotNull<const Common::Playlist*>());
    MOCK_CONST_METHOD0(artists, Array<Unique<Common::Artist>>());
    MOCK_CONST_METHOD0(musicalKeys, Array<Unique<Common::MusicalKey>>());
    MOCK_CONST_METHOD0(tags, Array<Unique<Common::Tag>>());
    MOCK_CONST_METHOD0(propertyTypes, Array<Common::Property::TypeID>());

    MOCK_CONST_METHOD1(maybeExistingTrackWithRelativeFilePath, Optional<NotNull<const Common::Track*>>(const FilePath& path));
};

using StrictMockMutableCollection = testing::StrictMock<MockMutableCollection>;

} }
