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

#include <CommonCollection/Artist.hpp>

#include "PlaylistTests.hpp"
#include "../CollectionTests.hpp"

#include <CommonCollection/Crates/Playlist.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace Common {

class PlaylistTests : public NxA::Test
{

};

TEST_F(PlaylistTests, numberOfCratesAndTrackEntriesContainedWithin_APlaylist_ReturnsTheCorrectValue)
{
    // -- Given.
    auto collection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ "/Volumes/Test"_String } },
                                                                       "Other Collection"_String,
                                                                       Common::Collection::Type::TrackFile);
    auto track1 = MockTrack::strictMockTrackInCollection(collection.asRawPointer(),
                                                         "This New Title"_String,
                                                         FilePath{ "Factory Sounds/some track.mp3"_String },
                                                         *Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2000-2-2 04:25:56", Time::defaultStringFormat));
    auto track2 = MockTrack::strictMockTrackInCollection(collection.asRawPointer(),
                                                         "This Other Better Title"_String,
                                                         FilePath{ "Factory Sounds/some track2.mp3"_String },
                                                         *Time::maybeTimeFromStringInLocalTimeZoneUsingFormat("2010-7-2 04:25:56", Time::defaultStringFormat));
    auto playlistSource = Shared<MockSubCrateSource>::with("Test Playlst"_String,
                                                           Array<Shared<MockSubCrateSource>>{ },
                                                           Array<NotNull<const Common::Track*>>{ NotNull<const Common::Track*>{ track1.asRawPointer() },
                                                                                                 NotNull<const Common::Track*>{ track2.asRawPointer() }
                                                           });
    auto playlist = Common::MockPlaylist::strictMockPlaylistInCollection(collection.asRawPointer(), playlistSource.asRawPointer());

    // -- When.
    auto result = playlist->numberOfCratesAndTrackEntriesContainedWithin();

    // -- Then.
    EXPECT_EQ(result, 3u);
}

} }
