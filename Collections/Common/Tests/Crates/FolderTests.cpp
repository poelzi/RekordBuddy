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

#include "FolderTests.hpp"
#include "../CollectionTests.hpp"

#include <CommonCollection/Crates/Folder.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace Common {

class FolderTests : public NxA::Test
{

};

TEST_F(FolderTests, numberOfCratesAndTrackEntriesContainedWithin_AFolder_ReturnsTheCorrectValue)
{
    // -- Given.
    auto collection = Common::MockCollection::strickMockCollectionWith(Volume{ FilePath{ "/Volumes/Test"_String } },
                                                                       "Other Collection"_String,
                                                                       Common::Collection::Type::TrackFile);
    auto folder = Common::MockFolder::strictMockFolderInCollection(collection.asRawPointer());

    // -- When.
    auto result = folder->numberOfCratesAndTrackEntriesContainedWithin();

    // -- Then.
    EXPECT_EQ(result, 9u);
}

} }
