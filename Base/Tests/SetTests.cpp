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

#include <Base/Set.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class SetTests : public NxA::Test
{

};

TEST_F(SetTests, SetWith_ASetWithStrings_ReturnsAnSetWithTheSameObjectsAsTheSource)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));

    // -- When.
    auto result = Set<String>(test);

    // -- Then.
    EXPECT_EQ(1u, result.length());
    EXPECT_EQ(test.anyObject(), result.anyObject());
}

TEST_F(SetTests, SetWith_AnEmptySet_ReturnsAnEmptySet)
{
    // -- Given.
    MutableSet<String> test;

    // -- When.
    auto result = Set<String>(test);

    // -- Then.
    EXPECT_EQ(0u, result.length());
}

TEST_F(SetTests, OperatorSquareBrackets_AccessOnConstantSet_ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    MutableSet<String> test;
    test.add(String("Test"));
    Set<String> constTest(test);

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_STREQ("Test", constTest.anyObject().asUTF8());
}

TEST_F(SetTests, add_AddingOneObject_AddsObjectCorrectly)
{
    // -- Given.
    MutableSet<String> test;

    // -- When.
    test.add(String("Test"));

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_EQ(::strcmp(test.anyObject().asUTF8(), "Test"), 0);
}

TEST_F(SetTests, OperatorEqual_TwoDifferentSets_ReturnFalse)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));
    MutableSet<String> test2;
    test2.add(String("Test"));
    test2.add(String("OtherTest2"));

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == test2);
}

TEST_F(SetTests, OperatorEqual_TwoEqualSets_ReturnTrue)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));
    MutableSet<String> test2;
    test2.add(String("Test"));
    test2.add(String("Test2"));

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test2);
}

TEST_F(SetTests, RemoveAll_MutableSetWithTwoObject_AssignCopyStillHasElements)
{
    // -- Given.
    MutableSet<String> test1;
    test1.add(String("Test"));
    test1.add(String("Test2"));
    Set<String> test2;
    test2 = test1;

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(SetTests, RemoveAll_ImmutableSetWithTwoObject_AssignCopyStillHasElements)
{
    // -- Given.
    MutableSet<String> test1;
    Set<String> test2;
    test1.add(String("Test"));
    test1.add(String("Test2"));
    test2 = test1;

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(SetTests, RemoveAll_MutableSetWithTwoObject_CopyStillHasElements)
{
    // -- Given.
    MutableSet<String> test1;
    test1.add(String("Test"));
    test1.add(String("Test2"));
    Set<String> test2{ test1 };

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(SetTests, RemoveAll_ImmutableSetWithTwoObject_CopyStillHasElements)
{
    // -- Given.
    MutableSet<String> test1;
    test1.add(String("Test"));
    test1.add(String("Test2"));
    MutableSet<String> test2{ test1 };

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(SetTests, Length_EmptySet_LengthReturnsZero)
{
    // -- Given.
    // -- When.
    MutableSet<String> test;

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

TEST_F(SetTests, RemoveAll_SetWithTwoObject_RemovesAllObjects)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));

    // -- When.
    test.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

TEST_F(SetTests, Find_ObjectInTheMiddleWithSameValue_ReturnsPositionCorrectly)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));
    test.add(String("Test3"));

    // -- When.
    auto position = test.find(String("Test2"));

    // -- Then.
    EXPECT_TRUE(position != test.end());
}

TEST_F(SetTests, Contains_ObjectInTheSetWithSameValue_ReturnsTrue)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));
    auto object3 = String("Test2");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.contains(object3));
}

TEST_F(SetTests, Contains_ObjectAlreadyInTheSet_ReturnsTrue)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.contains(String("Test2")));
}

TEST_F(SetTests, Contains_ObjectNotInTheSet_ReturnsFalse)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));
    auto object3 = String("Test3");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.contains(object3));
}

TEST_F(SetTests, addingObjectCausedAnInsertion_ObjectAlreadyInTheSet_ReturnsFalse)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));

    // -- When.
    auto result = test.addingObjectCausedAnInsertion(String("Test2"));

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(SetTests, addingObjectCausedAnInsertion_ObjectNotInTheSet_ReturnsTrue)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));

    // -- When.
    auto result = test.addingObjectCausedAnInsertion(String("Test3"));

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(SetTests, Find_ObjectInEndWithSameObject_ReturnsPositionCorrectly)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test"));
    test.add(String("Test2"));
    test.add(String("Test3"));

    // -- When.
    auto position = test.find(String("Test3"));

    // -- Then.
    EXPECT_TRUE(position != test.end());
}

TEST_F(SetTests, IntersectWith_AnotherSetWithComonObjects_ReturnsIntersection)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test1"));
    test.add(String("Test2"));
    test.add(String("Test3"));
    MutableSet<String> test2Mut;
    test2Mut.add(String("Test4"));
    test2Mut.add(String("Test5"));
    test2Mut.add(String("Test2"));
    test2Mut.add(String("Test7"));
    Set<String> test2{ std::move(test2Mut) };

    // -- When.
    test.intersectWith(test2);

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_TRUE(test.contains(String("Test2")));
}

TEST_F(SetTests, IntersectWith_AnotherSetWithNoComonObjects_ReturnsEmptyIntersection)
{
    // -- Given.
    MutableSet<String> test;
    test.add(String("Test1"));
    test.add(String("Test2"));
    test.add(String("Test3"));
    MutableSet<String> test2Mut;
    test2Mut.add(String("Test4"));
    test2Mut.add(String("Test5"));
    test2Mut.add(String("Test8"));
    test2Mut.add(String("Test7"));
    Set<String> test2{ std::move(test2Mut) };

    // -- When.
    test.intersectWith(test2);

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

}
