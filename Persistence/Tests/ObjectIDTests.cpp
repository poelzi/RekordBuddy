/
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

#include <Base/Test.hpp>
#include <RekordBuddyCollectionImplementationPersistence/RekordBuddySchema.hpp>
#include <RekordBuddyCollectionImplementationPersistence/Persistence.hpp>

using namespace testing;

namespace NxA { namespace RekordBuddy { namespace CollectionImplementation {

class ObjectIDTests : public NxA::Test
{

};

TEST_F(ObjectIDTests, ClassName_ObjectID_TestEquality)
{
    // -- Given.
    ObjectID obj0{ObjectID::idWithIndex<PersistentArtist>(1)};
    ObjectID obj1{ObjectID::idWithIndex<PersistentArtist>(1)};

    // -- When.
    // -- Then.
    EXPECT_TRUE(obj0 == obj1);
}

TEST_F(ObjectIDTests, ClassName_ObjectID_TestTypeInequality)
{
    // -- Given.
    ObjectID obj0{ObjectID::idWithIndex<PersistentArtist>(1)};
    ObjectID obj1{ObjectID::idWithIndex<PersistentTrack>(1)};

    // -- When.
    // -- Then.
    EXPECT_TRUE(obj0 != obj1);
}

TEST_F(ObjectIDTests, ClassName_ObjectID_TestInequality)
{
    // -- Given.
    ObjectID obj0{ObjectID::idWithIndex<PersistentArtist>(1)};
    ObjectID obj1{ObjectID::idWithIndex<PersistentArtist>(2)};

    // -- When.
    // -- Then.
    EXPECT_TRUE(obj0 != obj1);
}

TEST_F(ObjectIDTests, ClassName_ObjectID_TestComparison)
{
    // -- Given.
    ObjectID obj0{ObjectID::idWithIndex<PersistentArtist>(2)};
    ObjectID obj1{ObjectID::idWithIndex<PersistentArtist>(3)};

    // -- When.
    // -- Then.
    EXPECT_TRUE(obj0 < obj1);
    EXPECT_FALSE(obj0 > obj1);
}

} } }
