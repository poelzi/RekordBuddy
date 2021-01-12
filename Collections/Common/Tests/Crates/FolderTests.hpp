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

#include <CommonCollection/Crates/Folder.hpp>

#include "PlaylistTests.hpp"

#include <Base/Test.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class MockFolder;
using StrictMockFolder = testing::StrictMock<MockFolder>;

// -- This class mocks a Common Folder
class MockFolder : public Folder
{
    // -- Friends
    friend StrictMockFolder;

    // -- Types
    using SubCrate = Variant<Unique<StrictMockPlaylist>, Unique<StrictMockFolder>>;

    // -- Private Instance Variables
    Shared<MockSubCrateSource> source;
    MutableArray<Unique<StrictMockTrack>> trackSources;
    MutableArray<SubCrate> subCratesSources;

    // -- Private Constructors & Destructors
    MockFolder(NotNull<const Collection*> collection, Shared<MockSubCrateSource> folderSource, Optional<CratePath> maybeParentPath = nothing) : source{ folderSource }
    {
        NXA_DEFAULT_RETURN_ON_CALL(*this, collection(), collection);

        NXA_DEFAULT_RETURN_ON_CALL(*this, name(), this->source->name);

        auto path = maybeParentPath.isValid() ? maybeParentPath->pathForChildNamed(this->source->name) : CratePath::forCrateNamed(this->source->name);
        NXA_DEFAULT_RETURN_ON_CALL(*this, path(), path);

        NXA_DEFAULT_RETURN_ON_CALL(*this, lastModificationTime(), *Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2021-11-27 04:54:16", Time::defaultStringFormat));

        auto numberOfSubCrates = this->source->subCrateSources.length();
        NXA_DEFAULT_RETURN_ON_CALL(*this, numberOfSubCrates(), numberOfSubCrates);

        for (count subCrateIndex = 0; subCrateIndex < numberOfSubCrates; ++subCrateIndex) {
            auto subCrateSource = this->source->subCrateSources[subCrateIndex];

            if (subCrateSource->tracks.length()) {
                auto newSubcrate = MockPlaylist::strictMockPlaylistInCollection(collection, subCrateSource.asRawPointer(), path);

                NXA_DEFAULT_RETURN_ON_CALL(*this, subCrateAtIndex(subCrateIndex), Common::SubCrate{ NotNull<const Common::Playlist*>{ newSubcrate.asRawPointer() } });

                subCratesSources.append(std::move(newSubcrate));
            }
            else {
                auto newSubcrate = Unique<StrictMockFolder>::with(collection, subCrateSource, path);

                NXA_DEFAULT_RETURN_ON_CALL(*this, subCrateAtIndex(subCrateIndex), Common::SubCrate{ NotNull<const Common::Folder*>{ newSubcrate.asRawPointer() } });

                subCratesSources.append(std::move(newSubcrate));
            }
        }

        NXA_DEFAULT_INVOKE_ON_CALL(*this, tracks(), [this]() {
            return Common::Folder::tracksIn<NotNull<const Common::Track*>>(*this);
        });
    }

public:
    // -- Factory Methods
    static Unique<StrictMockFolder> strictMockFolderInCollection(NotNull<const Collection*> collection,
                                                                 String withName = "Test Folder"_String)
    {
        auto track1 = MockTrack::strictMockTrackInCollection(collection,
                                                             "This New Title"_String,
                                                             FilePath{ "Factory Sounds/some track.mp3"_String },
                                                             *Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2000-2-2 04:25:56", Time::defaultStringFormat));
        auto track2 = MockTrack::strictMockTrackInCollection(collection,
                                                             "This Other Better Title"_String,
                                                             FilePath{ "Factory Sounds/some track2.mp3"_String },
                                                             *Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2010-7-2 04:25:56", Time::defaultStringFormat));

        auto newFolder = Unique<StrictMockFolder>::with(collection,
                                                        Shared<MockSubCrateSource>::with(withName,
                                                                                         Array<Shared<MockSubCrateSource>>{ Shared<MockSubCrateSource>::with("Test Playlst"_String,
                                                                                                                                                             Array<Shared<MockSubCrateSource>>{ },
                                                                                                                                                             Array<NotNull<const Common::Track*>>{ NotNull<const Common::Track*>{ track1.asRawPointer() },
                                                                                                                                                                                                   NotNull<const Common::Track*>{ track2.asRawPointer() }
                                                                                                                                                             }),
                                                                                                                            Shared<MockSubCrateSource>::with("Test Other Folder"_String,
                                                                                                                                                             Array<Shared<MockSubCrateSource>>{ Shared<MockSubCrateSource>::with("Test Other Playlst"_String,
                                                                                                                                                                                                                                 Array<Shared<MockSubCrateSource>>{ },
                                                                                                                                                                                                                                 Array<NotNull<const Common::Track*>>{ NotNull<const Common::Track*>{ track2.asRawPointer() } }),
                                                                                                                                                                                                Shared<MockSubCrateSource>::with("One last playlist"_String,
                                                                                                                                                                                                                                 Array<Shared<MockSubCrateSource>>{ },
                                                                                                                                                                                                                                 Array<NotNull<const Common::Track*>>{ NotNull<const Common::Track*>{ track1.asRawPointer() } })
                                                                                                                                                             },
                                                                                                                                                             Array<NotNull<const Common::Track*>>{ }),
                                                                                         },
                                                                                         Array<NotNull<const Common::Track*>>{ }));

        newFolder->trackSources.emplaceAppend(std::move(track1));
        newFolder->trackSources.emplaceAppend(std::move(track2));

        return newFolder;
    }

    // -- Mocked Instance Methods
    MOCK_CONST_METHOD0(collection, NotNull<const Collection*>());

    MOCK_CONST_METHOD0(maybeParentFolder, Optional<NotNull<const Common::Folder*>>());

    MOCK_CONST_METHOD0(lastModificationTime, Time());
    MOCK_CONST_METHOD0(name, String());
    MOCK_CONST_METHOD0(path, CratePath());

    MOCK_CONST_METHOD0(numberOfSubCrates, count());
    MOCK_CONST_METHOD1(subCrateAtIndex, Common::SubCrate(count));

    MOCK_CONST_METHOD0(tracks, Array<NotNull<const Track*>>());
};

} }
