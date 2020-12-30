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

#include <Base/Map.hpp>
#include <Base/Test.hpp>
#include <Base/Array.hpp>

using namespace testing;

namespace NxA {

static String mapTestsTestString("Test String");
static String mapTestsOtherString("Other String");
    
class MapTests : public NxA::Test
{

};

TEST_F(MapTests, SetValueForKey_IntegerValue_SetsCorrectValue)
{
    // -- Given.
    static const uinteger32 integerValue = 72;
    MutableMap<uinteger32, uinteger32> test;

    // -- When.
    test.setValueForKey(integerValue, 0);

    // -- Then.
    EXPECT_EQ(integerValue, test.valueForKey(0));
}

TEST_F(MapTests, SetValueForKey_ValueFromAPointer_SetsCorrectValue)
{
    // -- Given.
    static const character* otherTestString = "testString";
    MutableMap<uinteger32, String> test;

    // -- When.
    test.setValueForKey(String{ otherTestString }, 0);

    // -- Then.
    EXPECT_STREQ(otherTestString, test.valueForKey(0).asUTF8());
}

TEST_F(MapTests, SetValueForKey_ValueFromAReference_SetsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;

    // -- When.
    test.setValueForKey(mapTestsTestString, 0);

    // -- Then.
    EXPECT_STREQ(mapTestsTestString.asUTF8(), test.valueForKey(0).asUTF8());
}

TEST_F(MapTests, SetValueForKey_ValueFromAPointerOverAnExistingValue_SetsCorrectValue)
{
    // -- Given.
    static const character* otherTestString = "testString";
    MutableMap<uinteger32, String> test;
    test.setValueForKey("Initial String"_String, 44);

    // -- When.
    test.setValueForKey(String{ otherTestString }, 44);

    // -- Then.
    EXPECT_STREQ(otherTestString, test.valueForKey(44).asUTF8());
}

TEST_F(MapTests, SetValueForKey_ValueFromAReferenceOverAnExistingValue_SetsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 44);

    // -- When.
    test.setValueForKey(mapTestsOtherString, 44);

    // -- Then.
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), test.valueForKey(44).asUTF8());
}

TEST_F(MapTests, SetValueForKeyCausedAnInsertion_IntegerValue_SetsCorrectValue)
{
    // -- Given.
    static const uinteger32 integerValue = 72;
    MutableMap<uinteger32, uinteger32> test;

    // -- When.
    auto existed = !test.setValueForKeyCausedAnInsertion(integerValue, 0);

    // -- Then.
    EXPECT_FALSE(existed);
    EXPECT_EQ(integerValue, test.valueForKey(0));
}

TEST_F(MapTests, SetValueForKeyCausedAnInsertion_ValueFromAPointer_SetsCorrectValue)
{
    // -- Given.
    static const character* otherTestString = "testString";
    MutableMap<uinteger32, String> test;

    // -- When.
    auto existed = !test.setValueForKeyCausedAnInsertion(String{ otherTestString }, 0);

    // -- Then.
    EXPECT_FALSE(existed);
    EXPECT_STREQ(otherTestString, test.valueForKey(0).asUTF8());
}

TEST_F(MapTests, SetValueForKeyCausedAnInsertion_ValueFromAReference_SetsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;

    // -- When.
    auto existed = !test.setValueForKeyCausedAnInsertion(mapTestsTestString, 0);

    // -- Then.
    EXPECT_FALSE(existed);
    EXPECT_STREQ(mapTestsTestString.asUTF8(), test.valueForKey(0).asUTF8());
}

TEST_F(MapTests, SetValueForKeyCausedAnInsertion_ValueFromAPointerOverAnExistingValue_SetsCorrectValue)
{
    // -- Given.
    static const character* otherTestString = "testString";
    MutableMap<uinteger32, String> test;
    test.setValueForKey("Initial String"_String, 44);

    // -- When.
    auto existed = !test.setValueForKeyCausedAnInsertion(String{ otherTestString }, 44);

    // -- Then.
    EXPECT_TRUE(existed);
    EXPECT_STREQ(otherTestString, test.valueForKey(44).asUTF8());
}

TEST_F(MapTests, SetValueForKeyCausedAnInsertion_ValueFromAReferenceOverAnExistingValue_SetsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 44);

    // -- When.
    auto existed = !test.setValueForKeyCausedAnInsertion(mapTestsOtherString, 44);

    // -- Then.
    EXPECT_TRUE(existed);
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), test.valueForKey(44).asUTF8());
}

TEST_F(MapTests, ValueForKey_IntegerValue_SetsCorrectValue)
{
    // -- Given.
    static const uinteger32 integerValue = 72;
    MutableMap<uinteger32, uinteger32> test;

    // -- When.
    test.valueForKey(0) = integerValue;

    // -- Then.
    EXPECT_EQ(integerValue, test.valueForKey(0));
}

TEST_F(MapTests, ValueForKey_ValueFromAPointer_SetsCorrectValue)
{
    // -- Given.
    static const character* otherTestString = "testString";
    MutableMap<uinteger32, String> test;

    // -- When.
    test.valueForKey(0) = String(otherTestString);

    // -- Then.
    EXPECT_STREQ(otherTestString, test.valueForKey(0).asUTF8());
}

TEST_F(MapTests, ValueForKey_ValueFromAReference_SetsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;

    // -- When.
    test.valueForKey(0) = mapTestsTestString;

    // -- Then.
    EXPECT_STREQ(mapTestsTestString.asUTF8(), test.valueForKey(0).asUTF8());
}

TEST_F(MapTests, ValueForKey_ValueFromAPointerOverAnExistingValue_SetsCorrectValue)
{
    // -- Given.
    static const character* otherTestString = "testString";
    MutableMap<uinteger32, String> test;
    test.setValueForKey("Initial String"_String, 44);

    // -- When.
    test.valueForKey(44) = String(otherTestString);

    // -- Then.
    EXPECT_STREQ(otherTestString, test.valueForKey(44).asUTF8());
}

TEST_F(MapTests, ValueForKey_ValueFromAReferenceOverAnExistingValue_SetsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 44);

    // -- When.
    test.valueForKey(44) = mapTestsOtherString;

    // -- Then.
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), test.valueForKey(44).asUTF8());
}

TEST_F(MapTests, ValueForKey_MapWithAGivenValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsTestString.asUTF8(), test.valueForKey(0x2323).asUTF8());
}

TEST_F(MapTests, ValueForKey_MapWithAnotherValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), test.valueForKey(0x2423).asUTF8());
}

TEST_F(MapTests, ValueForKey_LookingForAnUnknownKey_CreatesANewEntry)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    test.valueForKey(0x23);

    // -- Then.
    EXPECT_TRUE(test.maybeValueForKey(0x23) ? true : false);
    EXPECT_STREQ("", test.valueForKey(0x23).asUTF8());
}

TEST_F(MapTests, ValueForKey_ConstMapWithAGivenValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);
    const MutableMap<uinteger32, String>& constTest = test;

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsTestString.asUTF8(), constTest.valueForKey(0x2323).asUTF8());
}

TEST_F(MapTests, ValueForKey_ConstMapWithAnotherValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);
    const MutableMap<uinteger32, String>& constTest = test;

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), constTest.valueForKey(0x2423).asUTF8());
}

TEST_F(MapTests, ValueForKey_LookingForAnUnknownKeyInConstMap_ThrowsException)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);
    const MutableMap<uinteger32, String>& constTest = test;

    // -- When.
    // -- Then.
    EXPECT_THROW(constTest.valueForKey(0x23).asUTF8(), std::exception);
}

TEST_F(MapTests, OperatorSquareBrackets_MapWithAGivenValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsTestString.asUTF8(), test[0x2323].asUTF8());
}

TEST_F(MapTests, OperatorSquareBrackets_MapWithAnotherValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), test[0x2423].asUTF8());
}

TEST_F(MapTests, OperatorSquareBrackets_LookingForAnUnknownKey_ThrowsException)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_THROW(test[0x23].asUTF8(), std::exception);
}

TEST_F(MapTests, OperatorSquareBrackets_ConstMapWithAGivenValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);
    const MutableMap<uinteger32, String>& constTest = test;

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsTestString.asUTF8(), constTest[0x2323].asUTF8());
}

TEST_F(MapTests, OperatorSquareBrackets_ConstMapWithAnotherValue_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);
    const MutableMap<uinteger32, String>& constTest = test;

    // -- When.
    // -- Then.
    EXPECT_STREQ(mapTestsOtherString.asUTF8(), constTest[0x2423].asUTF8());
}

TEST_F(MapTests, OperatorSquareBrackets_LookingForAnUnknownKeyInConstMap_ThrowsException)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);
    const MutableMap<uinteger32, String>& constTest = test;

    // -- When.
    // -- Then.
    EXPECT_THROW(constTest[0x23].asUTF8(), std::exception);
}

TEST_F(MapTests, MaybeValueForKey_UnknownKey_ReturnsFalse)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.maybeValueForKey(0x23));
}

TEST_F(MapTests, MaybeValueForKey_KnownKey_ReturnsTrue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.maybeValueForKey(0x2323) ? true : false);
}

TEST_F(MapTests, MaybeValueForKey_EmptyMapWithArrayType_ReturnsNothing)
{
    // -- Given.
    Map<uinteger32, Array<String>> test;

    // -- When.
    auto result = test.maybeValueForKey(0x23);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(MapTests, MaybeValueForKey_MapWithArrayTypeUnknownKey_ReturnsNothing)
{
    // -- Given.
    Map<uinteger32, Array<String>> test{ { { 0x3, Array<String>{ "Test"_String, "Me"_String } },
                                           { 0x6, Array<String>{ "Test2"_String, "Me3"_String } } } };

    // -- When.
    auto result = test.maybeValueForKey(0x23);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(MapTests, MaybeValueForKey_MapWithArrayTypeKnownKey_ReturnsValue)
{
    // -- Given.
    Map<uinteger32, Array<String>> test{ { { 0x3, Array<String>{ "Test"_String, "Me"_String } },
                                           { 0x6, Array<String>{ "Test2"_String, "Me3"_String } } } };

    // -- When.
    auto result = test.maybeValueForKey(0x6);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->length(), 2u);
}

TEST_F(MapTests, MaybeValueForKey_EmptyMutableMapWithArrayType_ReturnsNothing)
{
    // -- Given.
    MutableMap<String, Array<String>> test;

    // -- When.
    auto result = test.maybeValueForKey("Test"_String);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(MapTests, MaybeValueForKey_MutableMapWithArrayTypeUnknownKey_ReturnsNothing)
{
    // -- Given.
    MutableMap<uinteger32, Array<String>> test{ { { 0x3, Array<String>{ "Test"_String, "Me"_String } },
                                                  { 0x6, Array<String>{ "Test2"_String, "Me3"_String } } } };

    // -- When.
    auto result = test.maybeValueForKey(0x23);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(MapTests, MaybeValueForKey_MutableMapWithArrayTypeKnownKey_ReturnsValue)
{
    // -- Given.
    MutableMap<uinteger32, Array<String>> test{ { { 0x3, Array<String>{ "Test"_String, "Me"_String } },
                                                  { 0x6, Array<String>{ "Test2"_String, "Me3"_String } } } };

    // -- When.
    auto result = test.maybeValueForKey(0x6);

    // -- Then.
    ASSERT_TRUE(result.isValid());
    EXPECT_EQ(result->length(), 2u);
}

TEST_F(MapTests, RemoveValueForKey_UnknownKey_ReturnsFalse)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    test.removeValueForKey(0x23);

    // -- Then.
    EXPECT_EQ(2u, test.length());
    EXPECT_TRUE(test.maybeValueForKey(0x2323) ? true : false);
    EXPECT_TRUE(test.maybeValueForKey(0x2423) ? true : false);
}

TEST_F(MapTests, RemoveValueForKey_KnownKey_ReturnsTrueAndRemovesTheKey)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    test.removeValueForKey(0x2323);

    // -- Then.
    EXPECT_EQ(1u, test.length());
    EXPECT_TRUE(test.maybeValueForKey(0x2423) ? true : false);
}

TEST_F(MapTests, RemoveValueForKeyCausedARemoval_UnknownKey_ReturnsFalse)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    auto existed = test.removeValueForKeyCausedARemoval(0x23);

    // -- Then.
    EXPECT_FALSE(existed);
    EXPECT_EQ(2u, test.length());
    EXPECT_TRUE(test.maybeValueForKey(0x2323) ? true : false);
    EXPECT_TRUE(test.maybeValueForKey(0x2423) ? true : false);
}

TEST_F(MapTests, RemoveValueForKeyCausedARemoval_KnownKey_ReturnsTrueAndRemovesTheKey)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    auto existed = test.removeValueForKeyCausedARemoval(0x2323);

    // -- Then.
    EXPECT_TRUE(existed);
    EXPECT_EQ(1u, test.length());
    EXPECT_TRUE(test.maybeValueForKey(0x2423) ? true : false);
}

TEST_F(MapTests, Length_MapWithKeys_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;
    test.setValueForKey(mapTestsTestString, 0x2323);
    test.setValueForKey(mapTestsOtherString, 0x2423);

    // -- When.
    // -- Then.
    EXPECT_EQ(2u, test.length());
}

TEST_F(MapTests, Length_MapWithNoKeys_ReturnsCorrectValue)
{
    // -- Given.
    MutableMap<uinteger32, String> test;

    // -- When.
    // -- Then.
    EXPECT_EQ(0u, test.length());
}

}
