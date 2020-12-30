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

#include "UserPreferencesTests.hpp"

#include <Base/Directory.hpp>
#include <Base/File.hpp>

using namespace testing;
using namespace NxA;

// -- Tests

TEST_F(UserPreferencesTests, maybeBooleanForKeyAndSetBooleanForKey_DifferentKeys_ReturnsTheCorrectValueForEachKey)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setBooleanForKey(true, "test1");
    userPreferences.setBooleanForKey(false, "test2");
    userPreferences.setIntegerForKey(true, "test3");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");
    auto maybeTest1 = userPreferences.maybeBooleanForKey("test1");
    auto maybeTest2 = userPreferences.maybeBooleanForKey("test2");
    auto maybeTest3 = userPreferences.maybeBooleanForKey("test3");

    // -- Then.
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_TRUE(*maybeTest1);

    ASSERT_TRUE(maybeTest2.isValid());
    EXPECT_FALSE(*maybeTest2);

    EXPECT_FALSE(maybeTest3.isValid());
}

TEST_F(UserPreferencesTests, setBooleanForKey_OverwritingAKey_ReturnsTheCorrectValueForTheKey)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setBooleanForKey(true, "test1");

    // -- When.
    userPreferences.setBooleanForKey(false, "test1");

    // -- Then.
    auto maybeTest1 = userPreferences.maybeBooleanForKey("test1");
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_FALSE(*maybeTest1);
}

TEST_F(UserPreferencesTests, setBooleanForKey_OverwritingAKeyWithNothing_ReturnsNothing)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setBooleanForKey(true, "test1");

    // -- When.
    userPreferences.setBooleanForKey(nothing, "test1");

    // -- Then.
    ASSERT_FALSE(userPreferences.maybeBooleanForKey("test1").isValid());
}

TEST_F(UserPreferencesTests, maybeIntegerForKeyAndSetIntegerForKey_DifferentKeys_ReturnsTheCorrectValueForEachKey)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setIntegerForKey(23, "test1");
    userPreferences.setIntegerForKey(-45, "test2");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");
    auto maybeTest1 = userPreferences.maybeIntegerForKey("test1");
    auto maybeTest2 = userPreferences.maybeIntegerForKey("test2");
    auto maybeTest3 = userPreferences.maybeIntegerForKey("test3");

    // -- Then.
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(*maybeTest1, 23);

    ASSERT_TRUE(maybeTest2.isValid());
    EXPECT_EQ(*maybeTest2, -45);

    EXPECT_FALSE(maybeTest3.isValid());
}

TEST_F(UserPreferencesTests, setIntegerForKey_OverWritingAKey_ReturnsTheCorrectValueForTheKey)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setIntegerForKey(23, "test1");

    // -- When.
    userPreferences.setIntegerForKey(0, "test1");

    // -- Then.
    auto maybeTest1 = userPreferences.maybeIntegerForKey("test1");
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(*maybeTest1, 0);
}

TEST_F(UserPreferencesTests, setIntegerForKey_OverWritingAKeyWithNothing_ReturnsNothing)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setIntegerForKey(23, "test1");

    // -- When.
    userPreferences.setIntegerForKey(nothing, "test1");

    // -- Then.
    ASSERT_FALSE(userPreferences.maybeIntegerForKey("test1").isValid());
}

TEST_F(UserPreferencesTests, maybeStringForKeyAndSetStringForKey_DifferentKeys_ReturnsTheCorrectValueForEachKey)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setStringForKey("yep"_String, "test1");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setIntegerForKey(23, "test3");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");
    auto maybeTest1 = userPreferences.maybeStringForKey("test1");
    auto maybeTest3 = userPreferences.maybeStringForKey("test3");

    // -- Then.
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_STREQ(maybeTest1->asUTF8(), "yep");
    EXPECT_FALSE(maybeTest3.isValid());
}

TEST_F(UserPreferencesTests, setStringForKey_OverwritingAKey_ReturnsTheCorrectValueForTheKey)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setStringForKey("yep"_String, "test1");

    // -- When.
    userPreferences.setStringForKey("nope"_String, "test1");

    // -- Then.
    auto maybeTest1 = userPreferences.maybeStringForKey("test1");
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_STREQ(maybeTest1->asUTF8(), "nope");
}

TEST_F(UserPreferencesTests, setStringForKey_OverwritingAKeyWithNothing_ReturnsNothing)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setStringForKey("yep"_String, "test1");

    // -- When.
    userPreferences.setStringForKey(nothing, "test1");

    // -- Then.
    ASSERT_FALSE(userPreferences.maybeStringForKey("test1").isValid());
}

TEST_F(UserPreferencesTests, setStringForKey_AndInvalidValue_DoesNotSetAnything)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setStringForKey("yep"_String, "test1");

    // -- When.
    userPreferences.setStringForKey(""_String, "test1");

    // -- Then.
    auto maybeTest1 = userPreferences.maybeStringForKey("test1");
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_STREQ(maybeTest1->asUTF8(), "yep");
}

TEST_F(UserPreferencesTests, maybeBlobForKeyAndSetBlobForKey_DifferentKeys_ReturnsTheCorrectValueForEachKey)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setIntegerForKey(23, "test3");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");
    auto maybeTest1 = userPreferences.maybeBlobForKey("test1");
    auto maybeTest3 = userPreferences.maybeBlobForKey("test3");

    // -- Then.
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(*maybeTest1, Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)));

    EXPECT_FALSE(maybeTest3.isValid());
}

TEST_F(UserPreferencesTests, setBlobForKey_OverwritingAKey_ReturnsTheCorrectValueForTheKey)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");
    byte otherBlobTestData[] = { 0x01, 0x23, 0x00, 0x03, 0x04, 0xff, 0x17 };

    // -- When.
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(otherBlobTestData, sizeof(otherBlobTestData)), "test1");

    // -- Then.
    auto maybeTest1 = userPreferences.maybeBlobForKey("test1");
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(*maybeTest1, Blob::withMemoryAndSize(otherBlobTestData, sizeof(otherBlobTestData)));
}

TEST_F(UserPreferencesTests, setBlobForKey_OverwritingAKeyWithNothing_ReturnsNothing)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");

    // -- When.
    userPreferences.setBlobForKey(nothing, "test1");

    // -- Then.
    ASSERT_FALSE(userPreferences.maybeBlobForKey("test1").isValid());
}

TEST_F(UserPreferencesTests, setBlobForKey_AndInvalidValue_DoesNotSetAnything)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");
    userPreferences.setBlobForKey(Blob{ }, "test1");

    // -- Then.
    auto maybeTest1 = userPreferences.maybeBlobForKey("test1");
    ASSERT_TRUE(maybeTest1.isValid());
    EXPECT_EQ(*maybeTest1, Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)));
}

TEST_F(UserPreferencesTests, maybeArrayOfStringsForKeyAndSetArrayOfStringsForKey_DifferentKeys_ReturnsTheCorrectValueForEachKey)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setIntegerForKey(23, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");
    auto result = userPreferences.arrayOfStringsForKey("test1");
    auto test3 = userPreferences.arrayOfStringsForKey("test3");

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_STREQ(result[0].asUTF8(), "one");
    EXPECT_STREQ(result[1].asUTF8(), "two");

    EXPECT_EQ(test3.length(), 0u);
}

TEST_F(UserPreferencesTests, maybeStringAtIndexForKey_DifferentKeysAndIndices_ReturnsTheCorrectValueForEach)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two", "three" }, "test1");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setIntegerForKey(23, "test3");

    // -- When.
    auto result1 = userPreferences.maybeStringAtIndexForKey(1, "test1");
    auto result2 = userPreferences.maybeStringAtIndexForKey(2, "test1");
    auto result3 = userPreferences.maybeStringAtIndexForKey(3, "test1");
    auto result4 = userPreferences.maybeStringAtIndexForKey(1, "test3");

    // -- Then.
    ASSERT_TRUE(result1.isValid());
    EXPECT_STREQ(result1->asUTF8(), "two");

    ASSERT_TRUE(result2.isValid());
    EXPECT_STREQ(result2->asUTF8(), "three");

    ASSERT_FALSE(result3.isValid());

    ASSERT_FALSE(result4.isValid());
}

TEST_F(UserPreferencesTests, setArrayOfStringsForKey_OverwritingAKey_ReturnsTheCorrectValueForTheKey)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");

    // -- When.
    userPreferences.setArrayOfStringsForKey(Array<String>{ "three", "four", "five" }, "test1");

    // -- Then.
    auto result = userPreferences.arrayOfStringsForKey("test1");
    ASSERT_EQ(result.length(), 3u);
    EXPECT_STREQ(result[0].asUTF8(), "three");
    EXPECT_STREQ(result[1].asUTF8(), "four");
    EXPECT_STREQ(result[2].asUTF8(), "five");
}

TEST_F(UserPreferencesTests, setArrayOfStringForKey_OverwritingAKeyWithNothing_ReturnsNothing)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");

    // -- When.
    userPreferences.setArrayOfStringsForKey(nothing, "test1");

    // -- Then.
    ASSERT_EQ(userPreferences.arrayOfStringsForKey("test1").length(), 0u);
}

TEST_F(UserPreferencesTests, maybeArrayOfIntegersForKeyAndSetArrayOfIntegersForKey_DifferentKeys_ReturnsTheCorrectValueForEachKey)
{
    // -- Given.
    UserPreferences userPreferences;

    // -- When.
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setIntegerForKey(23, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");
    auto result = userPreferences.arrayOfIntegersForKey("test3");
    auto test1 = userPreferences.arrayOfIntegersForKey("test1");

    // -- Then.
    ASSERT_EQ(result.length(), 2u);
    EXPECT_EQ(result[0], 424);
    EXPECT_EQ(result[1], -999);

    EXPECT_EQ(test1.length(), 0u);
}

TEST_F(UserPreferencesTests, maybeIntegerAtIndexForKey_DifferentKeysAndIndices_ReturnsTheCorrectValueForEach)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two", "three" }, "test1");
    userPreferences.setBooleanForKey(true, "test3");
    userPreferences.setStringForKey("yep"_String, "test3");
    userPreferences.setIntegerForKey(23, "test3");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test3");

    // -- When.
    auto result1 = userPreferences.maybeIntegerAtIndexForKey(1, "test3");
    auto result2 = userPreferences.maybeIntegerAtIndexForKey(1, "test1");

    // -- Then.
    ASSERT_TRUE(result1.isValid());
    EXPECT_EQ(result1, -999);

    ASSERT_FALSE(result2.isValid());
}

TEST_F(UserPreferencesTests, setArrayOfIntegersForKey_OverwritingAKey_ReturnsTheCorrectValueForTheKey)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 23, 465, -5 }, "test1");

    // -- When.
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 46, 90 }, "test1");

    // -- Then.
    auto result = userPreferences.arrayOfIntegersForKey("test1");
    ASSERT_EQ(result.length(), 2u);
    EXPECT_EQ(result[0], 46);
    EXPECT_EQ(result[1], 90);
}

TEST_F(UserPreferencesTests, setArrayOfIntegersForKey_OverwritingAKeyWithNothing_ReturnsNothing)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 23, 465, -5 }, "test1");

    // -- When.
    userPreferences.setArrayOfIntegersForKey(nothing, "test1");

    // -- Then.
    ASSERT_EQ(userPreferences.arrayOfIntegersForKey("test1").length(), 0u);
}

TEST_F(UserPreferencesTests, clearAll_KeysAlreadySet_ClearsAllTheKeys)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setStringForKey("yep"_String, "test1");
    userPreferences.setBooleanForKey(true, "test1");
    userPreferences.setBooleanForKey(false, "test2");
    userPreferences.setIntegerForKey(23, "test1");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");
    userPreferences.setArrayOfIntegersForKey(Array<integer>{ 424, -999 }, "test1");
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");

    // -- When.
    userPreferences.clearAll();

    // -- Then.
    auto maybeTest1String = userPreferences.maybeStringForKey("test1");
    ASSERT_FALSE(maybeTest1String.isValid());

    auto maybeTest1Boolean = userPreferences.maybeBooleanForKey("test1");
    ASSERT_FALSE(maybeTest1Boolean.isValid());

    auto maybeTest2Boolean = userPreferences.maybeBooleanForKey("test2");
    ASSERT_FALSE(maybeTest2Boolean.isValid());

    auto maybeTest1Integer = userPreferences.maybeIntegerForKey("test1");
    ASSERT_FALSE(maybeTest1Integer.isValid());

    auto test1ArrayOfStrings = userPreferences.arrayOfStringsForKey("test1");
    ASSERT_EQ(test1ArrayOfStrings.length(), 0u);

    auto test1ArrayOfIntegers = userPreferences.arrayOfIntegersForKey("test1");
    ASSERT_EQ(test1ArrayOfIntegers.length(), 0u);

    auto maybeTest1Blob = userPreferences.maybeBlobForKey("test1");
    ASSERT_FALSE(maybeTest1Blob.isValid());
}

TEST_F(UserPreferencesTests, loadAndMergeFromFileAndSaveToFile_KeysToSave_LoadsTheSameKeysBack)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setBooleanForKey(true, "test1");
    userPreferences.setBooleanForKey(false, "test2");
    userPreferences.setIntegerForKey(23, "test1");
    userPreferences.setStringForKey("yep"_String, "test1");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");
    auto tempFile = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("UserPreferencesTests.db"));
    File::deleteFileAt(tempFile);
    this->p_saveDefaultsToFile(userPreferences, tempFile);
    userPreferences.clearAll();

    // -- When.
    this->p_loadDefaultsFromFile(userPreferences, tempFile);
    File::deleteFileAt(tempFile);

    // -- Then.
    auto maybeTest1Boolean = userPreferences.maybeBooleanForKey("test1");
    ASSERT_TRUE(maybeTest1Boolean.isValid());
    EXPECT_TRUE(*maybeTest1Boolean);

    auto maybeTest2Boolean = userPreferences.maybeBooleanForKey("test2");
    ASSERT_TRUE(maybeTest2Boolean.isValid());
    EXPECT_FALSE(*maybeTest2Boolean);

    auto maybeTest1Integer = userPreferences.maybeIntegerForKey("test1");
    ASSERT_TRUE(maybeTest1Integer.isValid());
    EXPECT_EQ(*maybeTest1Integer, 23);

    auto maybeTest1String = userPreferences.maybeStringForKey("test1");
    ASSERT_TRUE(maybeTest1String.isValid());
    EXPECT_STREQ(maybeTest1String->asUTF8(), "yep");

    auto test1ArrayOfStrings = userPreferences.arrayOfStringsForKey("test1");
    ASSERT_EQ(test1ArrayOfStrings.length(), 2u);
    EXPECT_STREQ(test1ArrayOfStrings[0].asUTF8(), "one");
    EXPECT_STREQ(test1ArrayOfStrings[1].asUTF8(), "two");

    auto maybeTest1Blob = userPreferences.maybeBlobForKey("test1");
    ASSERT_TRUE(maybeTest1Blob.isValid());
    EXPECT_EQ(*maybeTest1Blob, Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)));
}

TEST_F(UserPreferencesTests, loadAndMergeFromFileAndSaveToFile_LoadingSomeKeysThatOverwriteSomeThatShouldRemain_LoadsTheNewKeysButLeavesTheOldOnes)
{
    // -- Given.
    UserPreferences userPreferences;
    userPreferences.setBooleanForKey(true, "test1");
    userPreferences.setBooleanForKey(false, "test2");
    userPreferences.setIntegerForKey(23, "test1");
    userPreferences.setStringForKey("yep"_String, "test1");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "one", "two" }, "test1");
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)), "test1");
    auto tempFile = FilePath::filePathByJoiningPaths(Directory::temporaryDirectory(), NXA_FILEPATH("UserPreferencesTests.db"));
    File::deleteFileAt(tempFile);
    this->p_saveDefaultsToFile(userPreferences, tempFile);
    userPreferences.clearAll();
    userPreferences.setBooleanForKey(false, "test1");
    userPreferences.setIntegerForKey(32, "test1");
    userPreferences.setStringForKey("nope"_String, "test1");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "three", "four", "five" }, "test1");
    byte otherBlobTestData[] = { 0x01, 0x23, 0xff, 0x17 };
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(otherBlobTestData, sizeof(otherBlobTestData)), "test1");
    userPreferences.setBooleanForKey(false, "2test1");
    userPreferences.setBooleanForKey(true, "2test2");
    userPreferences.setIntegerForKey(45, "2test1");
    userPreferences.setStringForKey("mayb"_String, "2test1");
    userPreferences.setArrayOfStringsForKey(Array<String>{ "seven", "eight" }, "2test1");
    byte yetAnotherBlobTestData[] = { 0x17 };
    userPreferences.setBlobForKey(Blob::withMemoryAndSize(yetAnotherBlobTestData, sizeof(yetAnotherBlobTestData)), "2test1");

    // -- When.
    this->p_loadDefaultsFromFile(userPreferences, tempFile);
    File::deleteFileAt(tempFile);

    // -- Then.
    auto maybeTest1Boolean = userPreferences.maybeBooleanForKey("test1");
    ASSERT_TRUE(maybeTest1Boolean.isValid());
    EXPECT_TRUE(*maybeTest1Boolean);

    auto maybeTest2Boolean = userPreferences.maybeBooleanForKey("test2");
    ASSERT_TRUE(maybeTest2Boolean.isValid());
    EXPECT_FALSE(*maybeTest2Boolean);

    auto maybeTest1Integer = userPreferences.maybeIntegerForKey("test1");
    ASSERT_TRUE(maybeTest1Integer.isValid());
    EXPECT_EQ(*maybeTest1Integer, 23);

    auto maybeTest1String = userPreferences.maybeStringForKey("test1");
    ASSERT_TRUE(maybeTest1String.isValid());
    EXPECT_STREQ(maybeTest1String->asUTF8(), "yep");

    auto test1ArrayOfStrings = userPreferences.arrayOfStringsForKey("test1");
    ASSERT_EQ(test1ArrayOfStrings.length(), 2u);
    EXPECT_STREQ(test1ArrayOfStrings[0].asUTF8(), "one");
    EXPECT_STREQ(test1ArrayOfStrings[1].asUTF8(), "two");

    auto maybeTest1Blob = userPreferences.maybeBlobForKey("test1");
    ASSERT_TRUE(maybeTest1Blob.isValid());
    EXPECT_EQ(*maybeTest1Blob, Blob::withMemoryAndSize(this->p_blobTestData, sizeof(this->p_blobTestData)));

    maybeTest1Boolean = userPreferences.maybeBooleanForKey("2test1");
    ASSERT_TRUE(maybeTest1Boolean.isValid());
    EXPECT_FALSE(*maybeTest1Boolean);

    maybeTest2Boolean = userPreferences.maybeBooleanForKey("2test2");
    ASSERT_TRUE(maybeTest2Boolean.isValid());
    EXPECT_TRUE(*maybeTest2Boolean);

    maybeTest1Integer = userPreferences.maybeIntegerForKey("2test1");
    ASSERT_TRUE(maybeTest1Integer.isValid());
    EXPECT_EQ(*maybeTest1Integer, 45);

    maybeTest1String = userPreferences.maybeStringForKey("2test1");
    ASSERT_TRUE(maybeTest1String.isValid());
    EXPECT_STREQ(maybeTest1String->asUTF8(), "mayb");

    test1ArrayOfStrings = userPreferences.arrayOfStringsForKey("2test1");
    ASSERT_EQ(test1ArrayOfStrings.length(), 2u);
    EXPECT_STREQ(test1ArrayOfStrings[0].asUTF8(), "seven");
    EXPECT_STREQ(test1ArrayOfStrings[1].asUTF8(), "eight");

    maybeTest1Blob = userPreferences.maybeBlobForKey("2test1");
    ASSERT_TRUE(maybeTest1Blob.isValid());
    EXPECT_EQ(*maybeTest1Blob, Blob::withMemoryAndSize(yetAnotherBlobTestData, sizeof(yetAnotherBlobTestData)));
}
