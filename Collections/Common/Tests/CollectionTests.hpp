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

#include <Base/Test.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class MockCollection;
using StrictMockCollection = testing::StrictMock<MockCollection>;

// -- This class mocks a Common Collection
class MockCollection : public Collection
{
    // -- Friends
    friend StrictMockCollection;

    // -- Private Constructors & Destructors
    MockCollection(Volume volume, String name, Common::Collection::Type type)
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, volume(), volume);
        NXA_DEFAULT_RETURN_ON_CALL(*this, name(), name);
        NXA_DEFAULT_RETURN_ON_CALL(*this, type(), type);
        NXA_DEFAULT_RETURN_ON_CALL(*this, mustHaveTracksOnTheSameVolume(), true);
        NXA_DEFAULT_RETURN_ON_CALL(*this, isOpened(), true);
    }

public:
    // -- Factory Methods
    static Unique<StrictMockCollection> strickMockCollectionWith(Volume volume, String name, Common::Collection::Type type)
    {
        return Unique<StrictMockCollection>::with(volume, name, type);
    }

    // -- Mocked Methods
    MOCK_CONST_METHOD0(p_description, String());

    MOCK_CONST_METHOD0(shouldBeOpenedLazily, boolean());
    MOCK_CONST_METHOD0(mustHaveTracksOnTheSameVolume, boolean());
    MOCK_CONST_METHOD0(allowsMovieTracksInPlaylists, boolean());

    MOCK_METHOD0(open, Optional<Common::Collection::Error>());
    MOCK_CONST_METHOD0(lastOpenResult, Optional<Common::Collection::Error>());

    MOCK_CONST_METHOD0(name, String());
    MOCK_CONST_METHOD0(iconName, const character*());
    MOCK_CONST_METHOD0(type, Common::Collection::Type());
    MOCK_CONST_METHOD0(volume, Volume());
    MOCK_CONST_METHOD0(lastModificationTime, Time());

    MOCK_CONST_METHOD0(isOpened, boolean());
    MOCK_CONST_METHOD0(maybeOpeningErrorDescription, Optional<String>());
    MOCK_CONST_METHOD0(hasChangesToSave, boolean());

    MOCK_CONST_METHOD0(artistsSeparator, const String&());
    MOCK_CONST_METHOD0(genresSeparator, const String&());
    MOCK_CONST_METHOD0(musicalKeysSeparator, const String&());

    MOCK_CONST_METHOD0(rootFolder, NotNull<const Common::Folder*>());
    MOCK_CONST_METHOD0(tracks, NotNull<const Common::Playlist*>());
    MOCK_CONST_METHOD0(artists, Array<Unique<Common::Artist>>());
    MOCK_CONST_METHOD0(musicalKeys, Array<Unique<Common::MusicalKey>>());
    MOCK_CONST_METHOD0(tags, Array<Unique<Common::Tag>>());
    MOCK_CONST_METHOD0(propertyTypes, Array<Common::Property::TypeID>());

    MOCK_CONST_METHOD1(maybeExistingTrackWithRelativeFilePath, Optional<NotNull<const Common::Track*>>(const FilePath&));
    MOCK_CONST_METHOD1(maybeExistingTrackWithAbsoluteFilePath, Optional<NotNull<const Common::Track*>>(const FilePath&));
};

} }
