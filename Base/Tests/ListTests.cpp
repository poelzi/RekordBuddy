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

#include <Base/List.hpp>
#include <Base/String.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class ListTests : public NxA::Test
{

};

TEST_F(ListTests, Constructor_AnInitializerListWithStrings_ReturnsAListWithTheSameStringsAsTheInitializerList)
{
    // -- Given.
    // -- When.
    List<String> test{ "Test", "Test2" };

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
}

TEST_F(ListTests, CopyConstructor_AListWithStrings_ReturnsAListWithTheSameStringsAsTheOtherList)
{
    // -- Given.
    List<String> test{ "Test", "Test2" };

    // -- When.
    auto result = List<String>{ test };

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_EQ(test[0], result[0]);
    EXPECT_EQ(test[1], result[1]);
}

TEST_F(ListTests, MoveConstructor_AListWithStrings_ReturnsAListWithTheSameStringsAsTheOtherList)
{
    // -- Given.
    List<String> test{ "Test", "Test2" };

    // -- When.
    auto result = List<String>{ std::move(test) };

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
}

TEST_F(ListTests, OperatorEqual_TwoEqualLists_ReturnsTrue)
{
    // -- Given.
    List<String> test1{ "Test", "Test2" };
    List<String> test2{ "Test", "Test2" };

    // -- When.
    // -- Then.
    EXPECT_TRUE(test1 == test2);
}

TEST_F(ListTests, OperatorEqual_TwoUnEqualLists_ReturnsFalse)
{
    // -- Given.
    List<String> test1{ "Test", "Test2" };
    List<String> test2{ "Test", "Test3" };

    // -- When.
    // -- Then.
    EXPECT_FALSE(test1 == test2);
}

TEST_F(ListTests, OperatorUnEqual_TwoEqualLists_ReturnsFalse)
{
    // -- Given.
    List<String> test1{ "Test", "Test2" };
    List<String> test2{ "Test", "Test2" };

    // -- When.
    // -- Then.
    EXPECT_FALSE(test1 != test2);
}

TEST_F(ListTests, OperatorUnEqual_TwoUnEqualLists_ReturnsTrue)
{
    // -- Given.
    List<String> test1{ "Test", "Test2" };
    List<String> test2{ "Test", "Test3" };

    // -- When.
    // -- Then.
    EXPECT_TRUE(test1 != test2);
}

TEST_F(ListTests, OperatorSquareBrackets_AList_ReturnsTheCorrectObject)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test[1];

    // -- Then.
    EXPECT_STREQ("Test2", result.asUTF8());
}

TEST_F(ListTests, OperatorSquareBrackets_AListAndAccessingInvalidIndices_Asserts)
{
    // -- Given.
    // -- When.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- Then.
    EXPECT_THROW(test[-1], NxA::AssertionFailed);
    EXPECT_THROW(test[3], NxA::AssertionFailed);
    EXPECT_THROW(test[23], NxA::AssertionFailed);
}

TEST_F(ListTests, OperatorSquareBrackets_AConstList_ReturnsTheCorrectObject)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test[1];

    // -- Then.
    EXPECT_STREQ("Test2", result.asUTF8());
}

TEST_F(ListTests, OperatorSquareBrackets_AConstListAndAccessingInvalidIndices_Asserts)
{
    // -- Given.
    // -- When.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- Then.
    EXPECT_THROW(test[-1], NxA::AssertionFailed);
    EXPECT_THROW(test[3], NxA::AssertionFailed);
    EXPECT_THROW(test[23], NxA::AssertionFailed);
}

TEST_F(ListTests, Begin_AList_ReturnsTheFirstObject)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = *test.begin();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, End_AList_ReturnsOnePastTheLastObject)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.end();

    // -- Then.
    EXPECT_STREQ("Test3", std::prev(result)->asUTF8());
}

TEST_F(ListTests, Begin_AnEmptyList_ReturnsTheSameAsEnd)
{
    // -- Given.
    List<String> test;

    // -- When.
    auto result = test.begin();

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Begin_AConstList_ReturnsTheFirstObject)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = *test.begin();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, End_AConstList_ReturnsOnePastTheLastObject)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test.end();

    // -- Then.
    EXPECT_STREQ("Test3", std::prev(result)->asUTF8());
}

TEST_F(ListTests, Begin_AConstEmptyList_ReturnsTheSameAsEnd)
{
    // -- Given.
    List<String> mutableTest;
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test.begin();

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Length_AList_ReturnsTheCorrectLength)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.length();

    // -- Then.
    EXPECT_EQ(3u, result);
}

TEST_F(ListTests, FirstObject_AList_ReturnsTheFirstObject)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.firstObject();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, FirstObject_AnEmptyList_Asserts)
{
    // -- Given.
    List<String> test;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.firstObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, FirstObject_AListWithOnlyOneObject_ReturnsTheSameAsLastObject)
{
    // -- Given.
    List<String> test{ "Test3" };

    // -- When.
    // -- Then.
    EXPECT_EQ(test.firstObject(), test.lastObject());
}

TEST_F(ListTests, FirstObject_AConstList_ReturnsTheFirstObject)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test.firstObject();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, FirstObject_AConstEmptyList_Asserts)
{
    // -- Given.
    List<String> mutableTest;
    const List<String>& test = mutableTest;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.firstObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, FirstObject_AConstListWithOnlyOneObject_ReturnsTheSameAsLastObject)
{
    // -- Given.
    List<String> mutableTest{ "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    // -- Then.
    EXPECT_EQ(test.firstObject(), test.lastObject());
}

TEST_F(ListTests, LastObject_AList_ReturnsTheLastObject)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.lastObject();

    // -- Then.
    EXPECT_STREQ("Test3", result.asUTF8());
}

TEST_F(ListTests, LastObject_AnEmptyList_Asserts)
{
    // -- Given.
    List<String> test;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.lastObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, LastObject_AConstList_ReturnsTheLastObject)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test.lastObject();

    // -- Then.
    EXPECT_STREQ("Test3", result.asUTF8());
}

TEST_F(ListTests, LastObject_AConstEmptyList_Asserts)
{
    // -- Given.
    List<String> mutableTest;
    const List<String>& test = mutableTest;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.lastObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, Contains_AListTestingForAnObjectItContains_ReturnTrue)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.contains("Test2");

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(ListTests, Contains_AListTestingForAnObjectItDoesNotContains_ReturnsFalse)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.contains("Test4");

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(ListTests, Find_AListTestingForAnObjectItContains_ReturnIteratorPointingToTheObject)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.find("Test2");

    // -- Then.
    EXPECT_STREQ("Test2", result->asUTF8());
}

TEST_F(ListTests, Contains_AListTestingForAnObjectItDoesNotContains_ReturnsEndIterator)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.find("Test4");

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Find_AConstListTestingForAnObjectItContains_ReturnIteratorPointingToTheObject)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test.find("Test2");

    // -- Then.
    EXPECT_STREQ("Test2", result->asUTF8());
}

TEST_F(ListTests, Contains_AConstListTestingForAnObjectItDoesNotContains_ReturnsEndIterator)
{
    // -- Given.
    List<String> mutableTest{ "Test", "Test2", "Test3" };
    const List<String>& test = mutableTest;

    // -- When.
    auto result = test.find("Test4");

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Constructor_AnInitializerMutableListWithStrings_ReturnsAMutableListWithTheSameStringsAsTheInitializerList)
{
    // -- Given.
    // -- When.
    MutableList<String> test{ "Test", "Test2" };

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
}

TEST_F(ListTests, CopyConstructor_AMutableListWithStrings_ReturnsAMutableListWithTheSameStringsAsTheOtherList)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };

    // -- When.
    auto result = MutableList<String>{ test };
    test.append("Other");       // -- We make sure the new list is not affected by what happens to the source one.

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_EQ(test[0], result[0]);
    EXPECT_EQ(test[1], result[1]);
}

TEST_F(ListTests, MoveConstructor_AMutableListWithStrings_ReturnsAMutableListWithTheSameStringsAsTheOtherList)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };

    // -- When.
    auto result = MutableList<String>{ std::move(test) };

    // -- Then.
    EXPECT_EQ(2u, result.length());
    EXPECT_STREQ(result[0].asUTF8(), "Test");
    EXPECT_STREQ(result[1].asUTF8(), "Test2");
}

TEST_F(ListTests, EqualOperator_TwoEqualMutableLists_ReturnsTrue)
{
    // -- Given.
    MutableList<String> test1{ "Test", "Test2" };
    MutableList<String> test2{ "Test", "Test2" };

    // -- When.
    // -- Then.
    EXPECT_TRUE(test1 == test2);
}

TEST_F(ListTests, EqualOperator_TwoUnEqualMutableLists_ReturnsFalse)
{
    // -- Given.
    MutableList<String> test1{ "Test", "Test2" };
    MutableList<String> test2{ "Test", "Test3" };

    // -- When.
    // -- Then.
    EXPECT_FALSE(test1 == test2);
}

TEST_F(ListTests, UnEqualOperator_TwoEqualMutableLists_ReturnsFalse)
{
    // -- Given.
    MutableList<String> test1{ "Test", "Test2" };
    MutableList<String> test2{ "Test", "Test2" };

    // -- When.
    // -- Then.
    EXPECT_FALSE(test1 != test2);
}

TEST_F(ListTests, UnEqualOperator_TwoUnEqualMutableLists_ReturnsTrue)
{
    // -- Given.
    MutableList<String> test1{ "Test", "Test2" };
    MutableList<String> test2{ "Test", "Test3" };

    // -- When.
    // -- Then.
    EXPECT_TRUE(test1 != test2);
}

TEST_F(ListTests, OperatorSquareBrackets_AMutableList_ReturnsTheCorrectObject)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test[1];

    // -- Then.
    EXPECT_STREQ("Test2", result.asUTF8());
}

TEST_F(ListTests, OperatorSquareBrackets_AMutableListAndAccessingInvalidIndices_Asserts)
{
    // -- Given.
    // -- When.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- Then.
    EXPECT_THROW(test[-1], NxA::AssertionFailed);
    EXPECT_THROW(test[3], NxA::AssertionFailed);
    EXPECT_THROW(test[23], NxA::AssertionFailed);
}

TEST_F(ListTests, OperatorSquareBrackets_AConstMutableList_ReturnsTheCorrectObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test[1];

    // -- Then.
    EXPECT_STREQ("Test2", result.asUTF8());
}

TEST_F(ListTests, OperatorSquareBrackets_AConstMutableListAndAccessingInvalidIndices_Asserts)
{
    // -- Given.
    // -- When.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- Then.
    EXPECT_THROW(test[-1], NxA::AssertionFailed);
    EXPECT_THROW(test[3], NxA::AssertionFailed);
    EXPECT_THROW(test[23], NxA::AssertionFailed);
}

TEST_F(ListTests, Begin_AMutableList_ReturnsTheFirstObject)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = *test.begin();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, End_AMutableList_ReturnsOnePastTheLastObject)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.end();

    // -- Then.
    EXPECT_STREQ("Test3", std::prev(result)->asUTF8());
}

TEST_F(ListTests, Begin_AnEmptyMutableList_ReturnsTheSameAsEnd)
{
    // -- Given.
    MutableList<String> test;

    // -- When.
    auto result = test.begin();

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Begin_AConstMutableList_ReturnsTheFirstObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = *test.begin();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, End_AConstMutableList_ReturnsOnePastTheLastObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test.end();

    // -- Then.
    EXPECT_STREQ("Test3", std::prev(result)->asUTF8());
}

TEST_F(ListTests, Begin_AConstEmptyMutableList_ReturnsTheSameAsEnd)
{
    // -- Given.
    MutableList<String> mutableTest;
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test.begin();

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Length_AMutableList_ReturnsTheCorrectLength)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.length();

    // -- Then.
    EXPECT_EQ(3u, result);
}

TEST_F(ListTests, FirstObject_AMutableList_ReturnsTheFirstObject)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.firstObject();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, FirstObject_AnEmptyMutableList_Asserts)
{
    // -- Given.
    MutableList<String> test;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.firstObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, FirstObject_AMutableListWithOnlyOneObject_ReturnsTheSameAsLastObject)
{
    // -- Given.
    MutableList<String> test{ "Test3" };

    // -- When.
    // -- Then.
    EXPECT_EQ(test.firstObject(), test.lastObject());
}

TEST_F(ListTests, FirstObject_AConstMutableList_ReturnsTheFirstObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test.firstObject();

    // -- Then.
    EXPECT_STREQ("Test", result.asUTF8());
}

TEST_F(ListTests, FirstObject_AConstEmptyMutableList_Asserts)
{
    // -- Given.
    MutableList<String> mutableTest;
    const MutableList<String>& test = mutableTest;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.firstObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, FirstObject_AConstMutableListWithOnlyOneObject_ReturnsTheSameAsLastObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    // -- Then.
    EXPECT_EQ(test.firstObject(), test.lastObject());
}

TEST_F(ListTests, LastObject_AMutableList_ReturnsTheLastObject)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.lastObject();

    // -- Then.
    EXPECT_STREQ("Test3", result.asUTF8());
}

TEST_F(ListTests, LastObject_AnEmptyMutableList_Asserts)
{
    // -- Given.
    MutableList<String> test;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.lastObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, LastObject_AConstMutableList_ReturnsTheLastObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test.lastObject();

    // -- Then.
    EXPECT_STREQ("Test3", result.asUTF8());
}

TEST_F(ListTests, LastObject_AConstEmptyMutableList_Asserts)
{
    // -- Given.
    MutableList<String> mutableTest;
    const MutableList<String>& test = mutableTest;

    // -- When.
    // -- Then.
    EXPECT_THROW(test.lastObject(), NxA::AssertionFailed);
}

TEST_F(ListTests, Contains_AMutableListTestingForAnObjectItContains_ReturnTrue)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.contains("Test2");

    // -- Then.
    EXPECT_TRUE(result);
}

TEST_F(ListTests, Contains_AMutableListTestingForAnObjectItDoesNotContains_ReturnsFalse)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.contains("Test4");

    // -- Then.
    EXPECT_FALSE(result);
}

TEST_F(ListTests, Find_AMutableListTestingForAnObjectItContains_ReturnIteratorPointingToTheObject)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.find("Test2");

    // -- Then.
    EXPECT_STREQ("Test2", result->asUTF8());
}

TEST_F(ListTests, Contains_AMutableListTestingForAnObjectItDoesNotContains_ReturnsEndIterator)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    auto result = test.find("Test4");

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, Find_AConstMutableListTestingForAnObjectItContains_ReturnIteratorPointingToTheObject)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test.find("Test2");

    // -- Then.
    EXPECT_STREQ("Test2", result->asUTF8());
}

TEST_F(ListTests, Contains_AConstMutableListTestingForAnObjectItDoesNotContains_ReturnsEndIterator)
{
    // -- Given.
    MutableList<String> mutableTest{ "Test", "Test2", "Test3" };
    const MutableList<String>& test = mutableTest;

    // -- When.
    auto result = test.find("Test4");

    // -- Then.
    EXPECT_EQ(test.end(), result);
}

TEST_F(ListTests, CopyConstructor_AMutableListConstructedFromAMutableList_ReturnsTheSameElements)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    MutableList<String> result{ test };
    test.append("Other");       // -- We make sure the new list is not affected by what happens to the source one.

    // -- Then.
    EXPECT_EQ(3u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
    EXPECT_STREQ("Test3", result[2].asUTF8());
}

TEST_F(ListTests, MoveConstructor_AMutableListConstructedFromAMutableList_ReturnsTheSameElements)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    MutableList<String> result{ std::move(test) };

    // -- Then.
    EXPECT_EQ(3u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
    EXPECT_STREQ("Test3", result[2].asUTF8());
}

TEST_F(ListTests, CopyConstructor_AMutableListConstructedFromAList_ReturnsTheSameElements)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    MutableList<String> result{ test };

    // -- Then.
    EXPECT_EQ(3u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
    EXPECT_STREQ("Test3", result[2].asUTF8());
}

TEST_F(ListTests, MoveConstructor_AMutableListConstructedFromAList_ReturnsTheSameElements)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    MutableList<String> result{ std::move(test) };

    // -- Then.
    EXPECT_EQ(3u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
    EXPECT_STREQ("Test3", result[2].asUTF8());
}

TEST_F(ListTests, CopyAssignment_AMutableListConstructedFromAList_ReturnsTheSameElements)
{
    // -- Given.
    List<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    MutableList<String> result = test;

    // -- Then.
    EXPECT_EQ(3u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
    EXPECT_STREQ("Test3", result[2].asUTF8());
}

TEST_F(ListTests, CopyAssignment_AMutableListConstructedFromAMutableList_ReturnsTheSameElements)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    MutableList<String> result = test;
    test.append("Other");       // -- We make sure the new list is not affected by what happens to the source one.

    // -- Then.
    EXPECT_EQ(3u, result.length());
    EXPECT_STREQ("Test", result[0].asUTF8());
    EXPECT_STREQ("Test2", result[1].asUTF8());
    EXPECT_STREQ("Test3", result[2].asUTF8());
}

TEST_F(ListTests, Append_AMutableListAndANewValue_ValueIsAppendedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };
    String value{ "Other" };

    // -- When.
    test.append(value);

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Other", test[2].asUTF8());
}

TEST_F(ListTests, Append_AMutableListAndANewMovedValue_ValueIsAppendedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };
    String value{ "Other" };

    // -- When.
    test.append(std::move(value));

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Other", test[2].asUTF8());
}

TEST_F(ListTests, EmplaceAppend_AMutableListAndANewValue_ValueIsAppendedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };

    // -- When.
    test.emplaceAppend("Other");

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Other", test[2].asUTF8());
}

TEST_F(ListTests, EmplaceAt_AMutableListAndANewValueInTheMiddle_ValueIsAddedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };

    // -- When.
    test.emplaceAt(std::next(test.begin()), "Other");

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Other", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(ListTests, EmplaceAt_AMutableListAndANewValueAtTheEnd_ValueIsAddedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };

    // -- When.
    test.emplaceAt(test.end(), "Other");

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Other", test[2].asUTF8());
}

TEST_F(ListTests, Append_AMutableListAndANewValueInTheMiddle_ValueIsAddedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };
    String value{ "Other" };

    // -- When.
    test.insertObjectAt(value, std::next(test.begin()));

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Other", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(ListTests, Append_AMutableListAndANewValueAtTheEnd_ValueIsAppendedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };
    String value{ "Other" };

    // -- When.
    test.insertObjectAt(value, test.end());

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Other", test[2].asUTF8());
}

TEST_F(ListTests, Append_AMutableListAndANewMovedValueInTheMiddle_ValueIsAddedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };
    String value{ "Other" };

    // -- When.
    test.insertObjectAt(std::move(value), std::next(test.begin()));

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Other", test[1].asUTF8());
    EXPECT_STREQ("Test2", test[2].asUTF8());
}

TEST_F(ListTests, Append_AMutableListAndANewMovedValueAtTheEnd_ValueIsAppendedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2" };
    String value{ "Other" };

    // -- When.
    test.insertObjectAt(std::move(value), test.end());

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test2", test[1].asUTF8());
    EXPECT_STREQ("Other", test[2].asUTF8());
}

TEST_F(ListTests, Remove_AMutableListAndAnObjectToRemoveThatIsInTheList_ObjectIsRemovedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    test.remove("Test2");

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(ListTests, Remove_AMutableListAndAnObjectToRemovedThatIsNotInTheList_Asserts)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };
    String value{ "Test4" };

    // -- When.
    // -- Then.
    EXPECT_THROW(test.remove(value), NxA::AssertionFailed);
}

TEST_F(ListTests, RemoveAll_AMutableListWithObjects_AllObjectsAreRemovedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    test.removeAll();

    // -- Then.
    EXPECT_EQ(0u, test.length());
}

TEST_F(ListTests, RemoveObjectAtIndex_AMutableListAndAnIndexThatIsInTheList_ObjectIsRemovedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    test.removeObjectAtIndex(1);

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(ListTests, RemoveObjectAtIndex_AMutableListAndAnIndexThatIsOutsideOfTheList_Asserts)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    // -- Then.
    EXPECT_THROW(test.removeObjectAtIndex(3), NxA::AssertionFailed);
}

TEST_F(ListTests, RemoveObjectAt_AMutableListAndAPositionThatIsInTheList_ObjectIsRemovedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "Test", "Test2", "Test3" };

    // -- When.
    test.removeObjectAt(test.begin());

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_STREQ("Test2", test[0].asUTF8());
    EXPECT_STREQ("Test3", test[1].asUTF8());
}

TEST_F(ListTests, Sort_AnUnsortedMutableList_ListIsSortedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "gTest2", "zTest2", "2Test3" };

    // -- When.
    test.sort();

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("2Test3", test[0].asUTF8());
    EXPECT_STREQ("gTest2", test[1].asUTF8());
    EXPECT_STREQ("zTest2", test[2].asUTF8());
}

TEST_F(ListTests, SortWith_AnUnsortedMutableListAndASortingFunction_ListIsSortedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "gTest2", "zTest2", "2Test3" };

    // -- When.
    test.sortWith([](String& first, String& second) {
        return ::strcmp(first.asUTF8(), second.asUTF8()) > 0;
    });

    // -- Then.
    EXPECT_EQ(3u, test.length());
    EXPECT_STREQ("zTest2", test[0].asUTF8());
    EXPECT_STREQ("gTest2", test[1].asUTF8());
    EXPECT_STREQ("2Test3", test[2].asUTF8());
}

TEST_F(ListTests, ReArrange_AnMutableListSomePositionsInThatListAndADestinationIndex_ObjectsInTheListAreMovedCorrectly)
{
    // -- Given.
    MutableList<String> test{ "gTest2", "zTest2", "Other", "2Test3" };

    // -- When.
    test.moveObjectsAtIndicesTo({ 0, 3 }, 2);

    // -- Then.
    EXPECT_EQ(4u, test.length());
    EXPECT_STREQ("zTest2", test[0].asUTF8());
    EXPECT_STREQ("gTest2", test[1].asUTF8());
    EXPECT_STREQ("2Test3", test[2].asUTF8());
    EXPECT_STREQ("Other", test[3].asUTF8());
}

TEST_F(ListTests, ReArrange_AnMutableListAndSomeInvalidDestinationPositions_Asserts)
{
    // -- Given.
    MutableList<String> test{ "gTest2", "zTest2", "Other", "2Test3" };

    // -- When.
    // -- Then.
    EXPECT_THROW(test.moveObjectsAtIndicesTo({ 0, 1, 2, 3, 4 }, 2), NxA::AssertionFailed);
    EXPECT_THROW(test.moveObjectsAtIndicesTo({ 0, 1, 2, 4 }, 2), NxA::AssertionFailed);
    EXPECT_THROW(test.moveObjectsAtIndicesTo({ 0, 1, 2 }, 5), NxA::AssertionFailed);
}

}
