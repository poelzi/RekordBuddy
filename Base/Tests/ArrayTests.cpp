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

#include <Base/Array.hpp>
#include <Base/String.hpp>
#include <Base/Set.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class ArrayTests : public NxA::Test
{

};

TEST_F(ArrayTests, Constructor_AnArrayWithStrings_ReturnsAnArrayWithTheSameObjectsAsTheSource)
{
    // -- Given.
    Array<String> test{ "Test", "Test2" };

    // -- When.
    auto result = Array<String>{ test };

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_EQ(test[0], result[0]);
    EXPECT_EQ(test[1], result[1]);
}

TEST_F(ArrayTests, Constructor_AnEmptyArray_ReturnsAnEmptyArray)
{
    // -- Given.
    Array<String> test;

    // -- When.
    auto result = Array<String>{ test };

    // -- Then.
    EXPECT_EQ(0u, result.length());
}

TEST_F(ArrayTests, Constructor_ASetOfStrings_ReturnsAnArrayWithTheSameObjectsAsTheSource)
{
    // -- Given.
    Set<String> test{ "Test", "Test2", "Test", "test3" };

    // -- When.
    auto result = Array<String>{ test };

    // -- Then.
    ASSERT_EQ(result.length(), 3u);
    EXPECT_EQ(result[0], "Test"_String);
    EXPECT_EQ(result[1], "Test2"_String);
    EXPECT_EQ(result[2], "test3"_String);
}

TEST_F(ArrayTests, OperatorSquareBrackets_AccessOnConstantArray_ReturnsCorrectValue)
{
    // -- Given.
    Array<String> source{ "Test" };
    const Array<String>& test = source;

    // -- When.
    auto result = test[0];

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_STREQ("Test", result.asUTF8());
}

class MutableArrayTests : public NxA::Test
{

};

TEST_F(MutableArrayTests, OperatorSquareBrackets_AccessOnMutableArrayOfMutable_ReturnsACopyOfTheMutableArray)
{
    // -- Given.
    MutableArray<MutableArray<String>> test;
    MutableArray<String> test2;
    test.append(test2);
    test2.append("Test");

    // -- When.
    auto result = test[0];

    // -- Then.
    EXPECT_EQ(0u, result.length());
    EXPECT_STREQ("Test", test2.firstObject().asUTF8());
}

TEST_F(MutableArrayTests, OperatorSquareBrackets_OutOfBoundsAccess_ThrowsException)
{
    // -- Given.
    MutableArray<String> test{ "Test" };

    // -- When.
    // -- Then.
    EXPECT_THROW(test[1], NxA::AssertionFailed);
    EXPECT_THROW(test[132], NxA::AssertionFailed);
}

TEST_F(MutableArrayTests, OperatorSquareBrackets_OutOfBoundsAccessOnConstantArray_ThrowsException)
{
    // -- Given.
    Array<String> source{ "Test" };
    const Array<String>& test = source;

    // -- When.
    // -- Then.
    EXPECT_THROW(test[1], NxA::AssertionFailed);
    EXPECT_THROW(test[132], NxA::AssertionFailed);
}

TEST_F(MutableArrayTests, Append_AddingOneObject_AddsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;

    // -- When.
    test.append("Test");

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_EQ(::strcmp(test[0].asUTF8(), "Test"), 0);
}

TEST_F(MutableArrayTests, Append_AddingTwoObject_AddsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;

    // -- When.
    test.append("Test");
    test.append("Test2");

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
}

TEST_F(MutableArrayTests, Prepend_AddingOneObject_AddsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;

    // -- When.
    test.prepend("Test");

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_EQ(::strcmp(test[0].asUTF8(), "Test"), 0);
}

TEST_F(MutableArrayTests, Prepend_AddingTwoObject_AddsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;

    // -- When.
    test.prepend("Test");
    test.prepend("Test2");

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test2", test[0].asUTF8());
    EXPECT_STREQ("Test", test[1].asUTF8());
}

TEST_F(MutableArrayTests, OperatorEqual_TwoDifferentArrays_ReturnFalse)
{
    // -- Given.
    MutableArray<String> test;
    test.append("Test");
    test.append("Test2");
    MutableArray<String> test2;
    test2.append("Test");
    test2.append("OtherTest2");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == test2);
}

TEST_F(MutableArrayTests, OperatorEqual_TwoEqualArrays_ReturnTrue)
{
    // -- Given.
    MutableArray<String> test;
    test.append("Test");
    test.append("Test2");
    MutableArray<String> test2;
    test2.append("Test");
    test2.append("Test2");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test2);
}

TEST_F(MutableArrayTests, Length_EmptyArray_LengthReturnsZero)
{
    // -- Given.
    // -- When.
    MutableArray<String> test;

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

TEST_F(MutableArrayTests, EmptyAll_MutableArrayWithTwoObject_AssignCopyStillHasElements)
{
    // -- Given.
    MutableArray<String> test1;
    Array<String> test2;
    test1.append("Test");
    test1.append("Test2");
    test2 = test1;

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(MutableArrayTests, EmptyAll_ImmutableArrayWithTwoObject_AssignCopyIsNotAffected)
{
    // -- Given.
    MutableArray<String> test1{ "Test", "Test2" };
    MutableArray<String> test2;
    test2 = test1;

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(MutableArrayTests, EmptyAll_MutableArrayWithTwoObject_CopyStillHasElements)
{
    // -- Given.
    MutableArray<String> test1{ "Test", "Test2" };
    auto test2 = test1.copy();

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(MutableArrayTests, EmptyAll_ImmutableArrayWithTwoObject_CopyStillHasElements)
{
    // -- Given.
    MutableArray<String> test1{ "Test", "Test2" };
    Array<String> test2{ test1 };

    // -- When.
    test1.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test1.length());
    EXPECT_EQ(2u, test2.length());
}

TEST_F(MutableArrayTests, EmptyAll_ArrayWithTwoObject_RemovesAllObjects)
{
    // -- Given.
    MutableArray<String> test{ "Test", "Test2" };

    // -- When.
    test.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

TEST_F(MutableArrayTests, InsertObjectAt_InsertingObjectAtTheBeginning_InsertsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test", "Test2" };

    // -- When.
    test.insertObjectAt("Test3", test.begin());

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test3", test[0].asUTF8());
    EXPECT_STREQ("Test", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(MutableArrayTests, InsertObjectAt_InsertingObjectAtTheEnd_InsertsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    test.append("Test");
    test.append("Test2");

    // -- When.
    test.insertObjectAt("Test3", test.end());

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Test3", test[2].asUTF8());
}

TEST_F(MutableArrayTests, InsertObjectAt_InsertingObjectInTheMiddle_InsertsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    test.append("Test");
    test.append("Test2");

    // -- When.
    test.insertObjectAt("Test3", test.begin() + 1);

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(MutableArrayTests, InsertObjectAtIndex_InsertingObjectAtTheBeginning_InsertsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test", "Test2" };

    // -- When.
    test.insertObjectAtIndex("Test3", 0);

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test3", test[0].asUTF8());
    EXPECT_STREQ("Test", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(MutableArrayTests, InsertObjectAtIndex_InsertingObjectAtTheEnd_InsertsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    test.append("Test");
    test.append("Test2");

    // -- When.
    test.insertObjectAtIndex("Test3", 2);

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Test3", test[2].asUTF8());
}

TEST_F(MutableArrayTests, InsertObjectAtIndex_InsertingObjectInTheMiddle_InsertsObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    test.append("Test");
    test.append("Test2");

    // -- When.
    test.insertObjectAtIndex("Test3", 1);

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(MutableArrayTests, Find_ObjectInTheMiddleWithSameValue_ReturnsPositionCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    auto position = test.find("Test2");

    // -- Then.
    EXPECT_TRUE(position != test.end());
    EXPECT_TRUE(position == (test.begin() + 1));
}

TEST_F(MutableArrayTests, FindStartingFromIndex_ObjectAfterTheIndexWithSameValue_ReturnsPositionCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    auto position = test.findStartingFromIndex("Test3", 2);

    // -- Then.
    EXPECT_TRUE(position != test.end());
    EXPECT_TRUE(position == (test.begin() + 2));
}

TEST_F(MutableArrayTests, FindStartingFromIndex_ObjectBeforeTheIndexWithSameValue_ReturnsNothing)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    auto position = test.findStartingFromIndex("Test2", 2);

    // -- Then.
    EXPECT_TRUE(position == test.end());
}

TEST_F(MutableArrayTests, FindStartingFromIndex_ObjectNotThere_ReturnsNothing)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    auto position = test.findStartingFromIndex("Test4", 1);

    // -- Then.
    EXPECT_TRUE(position == test.end());
}

TEST_F(MutableArrayTests, maybeIndexFor_ObjectInTheMiddleWithSameValue_ReturnsTheCorrectIndec)
{
    // -- Given.
    Array<String> test {
            "Test"_String,
            "Test2"_String,
            "Test3"_String,
    };

    // -- When.
    auto result = test.maybeIndexFor("Test2");

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 1u);
}

TEST_F(MutableArrayTests, maybeIndexFor_ObjectNotInTheArray_ReturnsNothing)
{
    // -- Given.
    Array<String> test {
            "Test"_String,
            "Test2"_String,
            "Test3"_String,
    };

    // -- When.
    auto result = test.maybeIndexFor("Testm");

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MutableArrayTests, Contains_ObjectInTheArrayWithSameValue_ReturnsTrue)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    test.append(object1);
    test.append(object2);
    auto object3 = String("Test2");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.contains(object3));
}

TEST_F(MutableArrayTests, Contains_ObjectAlreadyInTheArray_ReturnsTrue)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    test.append(object1);
    test.append(object2);

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.contains(object2));
}

TEST_F(MutableArrayTests, Contains_ObjectNotInTheArray_ReturnsFalse)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    test.append(object1);
    test.append(object2);
    auto object3 = String("Test3");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.contains(object3));
}

TEST_F(MutableArrayTests, Find_ObjectInEndWithSameObject_ReturnsPositionCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    auto position = test.find(object3);

    // -- Then.
    EXPECT_TRUE(position != test.end());
    EXPECT_TRUE(position == (test.begin() + 2));
}

TEST_F(MutableArrayTests, maybeIndexFor_ObjectInTheMiddleOfTheMutableArray_ReturnsTheCorrectIndec)
{
    // -- Given.
    MutableArray<String> test {
            "Test"_String,
            "Test2"_String,
            "Test3"_String,
    };

    // -- When.
    auto result = test.maybeIndexFor("Test2");

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(*result, 1u);
}

TEST_F(MutableArrayTests, maybeIndexFor_ObjectNotInTheMutableArray_ReturnsNothing)
{
    // -- Given.
    MutableArray<String> test {
            "Test"_String,
            "Test2"_String,
            "Test3"_String,
    };

    // -- When.
    auto result = test.maybeIndexFor("Testm");

    // -- Then.
    ASSERT_FALSE(result.isValid());
}

TEST_F(MutableArrayTests, RemoveObjectAt_PositionOfObjectInTheMiddle_RemovesObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test {
            "Test"_String,
            "Test2"_String,
            "Test3"_String,
    };

    // -- When.
    test.removeObjectAt(test.begin() + 1);

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(MutableArrayTests, RemoveObjectAtIndex_IndexOfObjectInTheMiddle_RemovesObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    test.removeObjectAtIndex(1);

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(MutableArrayTests, RemoveFirstObject_AnArrayWithObjects_RemovesTheFirstOne)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String };

    // -- When.
    test.removeFirstObject();

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test2", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(MutableArrayTests, RemoveLastObject_AnArrayWithObjects_RemovesTheLastOne)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String };

    // -- When.
    test.removeLastObject();

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
}

TEST_F(MutableArrayTests, Remove_ObjectInTheMiddle_RemovesObjectCorrectly)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("Test");
    auto object2 = String("Test2");
    auto object3 = String("Test3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    test.remove(object2);

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(MutableArrayTests, Sort_SortedArray_ReturnsUnchangedArray)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("ATest");
    auto object2 = String("BTest2");
    auto object3 = String("CTest3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    test.sort();

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("ATest", test[0].asUTF8());
    EXPECT_STREQ("BTest2", test[1].asUTF8());
    EXPECT_STREQ("CTest3", test[2].asUTF8());
}

TEST_F(MutableArrayTests, Sort_UnsortedArray_ReturnsASortedArray)
{
    // -- Given.
    MutableArray<String> test;
    auto object1 = String("CTest");
    auto object2 = String("ATest2");
    auto object3 = String("BTest3");
    test.append(object1);
    test.append(object2);
    test.append(object3);

    // -- When.
    test.sort();

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("ATest2", test[0].asUTF8());
    EXPECT_STREQ("BTest3", test[1].asUTF8());
    EXPECT_STREQ("CTest", test[2].asUTF8());
}

TEST_F(MutableArrayTests, MoveObjectAtIndexTo_MutableArrayWithObjects_MovesCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    test.moveObjectAtIndexTo(4, 1);

    // -- Then.
    ASSERT_EQ(test.length(), 5u);
    EXPECT_EQ(test[0], "Test"_String);
    EXPECT_EQ(test[1], "Test5"_String);
    EXPECT_EQ(test[2], "Test2"_String);
    EXPECT_EQ(test[3], "Test3"_String);
    EXPECT_EQ(test[4], "Test4"_String);
}

TEST_F(MutableArrayTests, MoveObjectAtIndexTo_MutableArrayWithObjectsAndSourceBeforeDest_MovesCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    test.moveObjectAtIndexTo(1, 5);

    // -- Then.
    ASSERT_EQ(test.length(), 5u);
    EXPECT_EQ(test[0], "Test"_String);
    EXPECT_EQ(test[1], "Test3"_String);
    EXPECT_EQ(test[2], "Test4"_String);
    EXPECT_EQ(test[3], "Test5"_String);
    EXPECT_EQ(test[4], "Test2"_String);
}

TEST_F(MutableArrayTests, moveObjectAtIndicesTo_MutableArrayWithObjects_MovesCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    test.moveObjectsAtIndicesTo({ 3, 4 }, 1);

    // -- Then.
    ASSERT_EQ(test.length(), 5u);
    EXPECT_EQ(test[0], "Test"_String);
    EXPECT_EQ(test[1], "Test4"_String);
    EXPECT_EQ(test[2], "Test5"_String);
    EXPECT_EQ(test[3], "Test2"_String);
    EXPECT_EQ(test[4], "Test3"_String);
}

TEST_F(MutableArrayTests, moveObjectAtIndicesTo_MutableArrayWithObjectsButIndicesThatOverlapTheDestination_MovesCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    test.moveObjectsAtIndicesTo({ 4, 2 }, 2);

    // -- Then.
    ASSERT_EQ(test.length(), 5u);
    EXPECT_EQ(test[0], "Test"_String);
    EXPECT_EQ(test[1], "Test2"_String);
    EXPECT_EQ(test[2], "Test5"_String);
    EXPECT_EQ(test[3], "Test3"_String);
    EXPECT_EQ(test[4], "Test4"_String);
}

TEST_F(MutableArrayTests, moveObjectAtIndicesTo_MutableArrayWithObjectsAndOnlyOneObjectToMove_MovesCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    test.moveObjectsAtIndicesTo({ 4 }, 1);

    // -- Then.
    ASSERT_EQ(test.length(), 5u);
    EXPECT_EQ(test[0], "Test"_String);
    EXPECT_EQ(test[1], "Test5"_String);
    EXPECT_EQ(test[2], "Test2"_String);
    EXPECT_EQ(test[3], "Test3"_String);
    EXPECT_EQ(test[4], "Test4"_String);
}

TEST_F(MutableArrayTests, moveObjectAtIndicesTo_MutableArrayWithObjectsAndOnlyOneObjectToMoveButSourceBeforeDest_MovesCorrectly)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    test.moveObjectsAtIndicesTo({ 1 }, 5);

    // -- Then.
    ASSERT_EQ(test.length(), 5u);
    EXPECT_EQ(test[0], "Test"_String);
    EXPECT_EQ(test[1], "Test3"_String);
    EXPECT_EQ(test[2], "Test4"_String);
    EXPECT_EQ(test[3], "Test5"_String);
    EXPECT_EQ(test[4], "Test2"_String);
}

TEST_F(MutableArrayTests, moveObjectAtIndicesTo_MutableArrayWithObjectsButIndexOutOfRange_Throws)
{
    // -- Given.
    MutableArray<String> test{ "Test"_String,
                               "Test2"_String,
                               "Test3"_String,
                               "Test4"_String,
                               "Test5"_String };

    // -- When.
    // -- Then.
    EXPECT_THROW(test.moveObjectsAtIndicesTo({ 3, 5 }, 1), AssertionFailed);
}

}
