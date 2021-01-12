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

#include <CommonCollection/Crates/Playlist.hpp>
#include <CommonCollection/Tracks/Tag.hpp>

#include "../Tracks/TrackTests.hpp"

#include <Base/Test.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class MockPlaylist;
using StrictMockPlaylist = testing::StrictMock<MockPlaylist>;

// -- This used to describe a mocked subcrate and its content.
class MockSubCrateSource
{
public:
    // -- Instance Variables
    String name;
    Array<Shared<MockSubCrateSource>> subCrateSources;
    Array<NotNull<const Common::Track*>> tracks;

    // -- Constructors & Destructors
    MockSubCrateSource(String name,
                       Array<Shared<MockSubCrateSource>> subCrateSources,
                       Array<NotNull<const Common::Track*>> tracks) : name{ name },
                                                                      subCrateSources{ subCrateSources },
                                                                      tracks{ tracks } { };
};

// -- This class mocks a Common Playlist
class MockPlaylist : public Playlist
{
    // -- Friends
    friend StrictMockPlaylist;

    // -- Private Constructors & Destructors
    MockPlaylist(NotNull<const Collection*> collection, NotNull<const MockSubCrateSource*> source, Optional<CratePath> maybeParentPath = nothing)
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, collection(), collection);

        NXA_DEFAULT_RETURN_ON_CALL(*this, name(), source->name);

        auto path = maybeParentPath.isValid() ? maybeParentPath->pathForChildNamed(source->name) : CratePath::forCrateNamed(source->name);
        NXA_DEFAULT_RETURN_ON_CALL(*this, path(), path);

        NXA_DEFAULT_RETURN_ON_CALL(*this, lastModificationTime(), *Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2021-11-27 04:54:16", Time::defaultStringFormat));

        NXA_DEFAULT_RETURN_ON_CALL(*this, iconName(), "SuperIcon.png");

        NXA_DEFAULT_RETURN_ON_CALL(*this, numberOfSubCrates(), 0);

        count numberOfTracks = source->tracks.length();
        NXA_DEFAULT_RETURN_ON_CALL(*this, numberOfTracks(), numberOfTracks);

        NXA_DEFAULT_RETURN_ON_CALL(*this, tracks(), source->tracks);

        for(count trackIndex = 0; trackIndex < numberOfTracks; ++trackIndex) {
            NXA_DEFAULT_RETURN_ON_CALL(*this, trackAtIndex(trackIndex), source->tracks[trackIndex].get());
        }
    }

public:
    // -- Factory Methods
    static Unique<StrictMockPlaylist> strictMockPlaylistInCollection(NotNull<const Collection*> collection,
                                                                     NotNull<const MockSubCrateSource*> source,
                                                                     Optional<CratePath> maybeParentPath = nothing)
    {
        return Unique<StrictMockPlaylist>::with(collection, source, maybeParentPath);
    }

    // -- Mocked Instance Methods
    MOCK_CONST_METHOD0(collection, NotNull<const Collection*>());

    MOCK_CONST_METHOD0(maybeParentFolder, Optional<NotNull<const Common::Folder*>>());

    MOCK_CONST_METHOD0(lastModificationTime, Time());
    MOCK_CONST_METHOD0(name, String());
    MOCK_CONST_METHOD0(path, CratePath());

    MOCK_CONST_METHOD0(iconName, const character*());

    MOCK_CONST_METHOD0(numberOfSubCrates, count());
    MOCK_CONST_METHOD1(subCrateAtIndex, SubCrate(count));

    MOCK_CONST_METHOD0(numberOfTracks, count());
    MOCK_CONST_METHOD1(trackAtIndex, NotNull<const Track*>(count));
    MOCK_CONST_METHOD0(tracks, Array<NotNull<const Track*>>());

    MOCK_CONST_METHOD1(isOrganizedBy, boolean(const Common::Tag&));
    MOCK_CONST_METHOD1(isOrganizedBy, boolean(Common::Property::TypeID));
};

} }
