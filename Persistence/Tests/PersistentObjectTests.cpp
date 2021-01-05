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

#include <RekordBuddyCollectionImplementationPersistence/RekordBuddySchema.hpp>
#include <RekordBuddyCollectionImplementationPersistence/Persistence.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace RekordBuddy { namespace CollectionImplementation {

class PersistentObjectTests : public NxA::Test
{

};

TEST_F(PersistentObjectTests, ClassName_PersistentObject_ObjectIDComparison)
{
    // -- Given.
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto context = std::make_shared<PersistentContext<RBSchema>>(store);
    context->setupAndMigrate();
    ObjectID objID{10, RBSchema::Type::Artist};
    PersistentObject test{objID, context};

    // -- When.
    auto testObjID = test.objectID();

    // -- Then.
    EXPECT_EQ(objID, testObjID);
    EXPECT_TRUE(objID.objectType() == testObjID.objectType());
    EXPECT_EQ(objID.objectIndex(), testObjID.objectIndex());
}

TEST_F(PersistentObjectTests, ClassName_PersistentObject_SaveToContext)
{
    // -- Given.
    auto store = std::make_shared<RBSchema::Store>(NXA_FILEPATH(":memory:"));
    auto context = std::make_shared<PersistentContext<RBSchema>>(store);
    context->setupAndMigrate();

    {
        // -- When.
        auto obj1 = context->createObject<PersistentArtist>();
        auto obj2 = context->createObject<PersistentArtist>();

        // -- Then.
        EXPECT_TRUE(obj2->objectID().objectType() == obj1->objectID().objectType());
        EXPECT_TRUE(obj2->objectID().objectIndex() > obj1->objectID().objectIndex());
        EXPECT_TRUE(obj2->objectID().objectSchemaVersion() == obj1->objectID().objectSchemaVersion());
        EXPECT_TRUE(obj2->objectID() > obj1->objectID());
    }
}

} } }
