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

#include <Base/String.hpp>
#include <Base/Blob.hpp>
#include <Base/Test.hpp>
#include <Base/Array.hpp>

using namespace testing;

namespace NxA {

static const byte stringTestsUTF16String[] = {
    0x00, 0x20, 0x00, 0x21, 0x00, 0x22, 0x00, 0x23, 0x00, 0x24, 0x00, 0x25, 0x00, 0x26, 0x00, 0x27, 0x00, 0x28, 0x00, 0x29, 0x00, 0x2A, 0x00, 0x2B,
    0x00, 0x2C, 0x00, 0x2D, 0x00, 0x2E, 0x00, 0x2F, 0x00, 0x30, 0x00, 0x31, 0x00, 0x32, 0x00, 0x33, 0x00, 0x34, 0x00, 0x35, 0x00, 0x36, 0x00, 0x37,
    0x00, 0x38, 0x00, 0x39, 0x00, 0x3A, 0x00, 0x3B, 0x00, 0x3C, 0x00, 0x3D, 0x00, 0x3E, 0x00, 0x3F, 0x00, 0x40, 0x00, 0x41, 0x00, 0x42, 0x00, 0x43,
    0x00, 0x44, 0x00, 0x45, 0x00, 0x46, 0x00, 0x47, 0x00, 0x48, 0x00, 0x49, 0x00, 0x4A, 0x00, 0x4B, 0x00, 0x4C, 0x00, 0x4D, 0x00, 0x4E, 0x00, 0x4F,
    0x00, 0x50, 0x00, 0x51, 0x00, 0x52, 0x00, 0x53, 0x00, 0x54, 0x00, 0x55, 0x00, 0x56, 0x00, 0x57, 0x00, 0x58, 0x00, 0x59, 0x00, 0x5A, 0x00, 0x5B,
    0x00, 0x5C, 0x00, 0x5D, 0x00, 0x5E, 0x00, 0x5F, 0x00, 0x60, 0x00, 0x61, 0x00, 0x62, 0x00, 0x63, 0x00, 0x64, 0x00, 0x65, 0x00, 0x66, 0x00, 0x67,
    0x00, 0x68, 0x00, 0x69, 0x00, 0x6A, 0x00, 0x6B, 0x00, 0x6C, 0x00, 0x6D, 0x00, 0x6E, 0x00, 0x6F, 0x00, 0x70, 0x00, 0x71, 0x00, 0x72, 0x00, 0x73,
    0x00, 0x74, 0x00, 0x75, 0x00, 0x76, 0x00, 0x77, 0x00, 0x78, 0x00, 0x79, 0x00, 0x7A, 0x00, 0x7B, 0x00, 0x7C, 0x00, 0x7D, 0x00, 0x7E
};

constexpr const character* stringTestsUTF8String = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

class StringTests : public NxA::Test
{

};

TEST_F(StringTests, StringContructor_StringCreatedFromCharPointer_ToUTF8ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    String test(stringTestsUTF8String);

    // -- Then.
    EXPECT_STREQ(stringTestsUTF8String, test.asUTF8());
}

TEST_F(StringTests, StringContructor_StringCreatedFromOtherString_ToUTF8ReturnsContainsCorrectValue)
{
    // -- Given.
    String test(stringTestsUTF8String);

    // -- When.
    auto otherTest = String(test);

    // -- Then.
    EXPECT_STREQ(stringTestsUTF8String, otherTest.asUTF8());
}

TEST_F(StringTests, StringWithUTF8_AUTF8String_ContainsCorrectValue)
{
    // -- Given.
    // -- When.
    auto test = String::stringWithUTF8(stringTestsUTF8String, String::UTF8Flag::IsNormalized);

    // -- Then.
    EXPECT_STREQ(stringTestsUTF8String, test.asUTF8());
}

TEST_F(StringTests, StringWithUTF8_AUTF8StringWhenNotNormalizing_ContainsNonNormalizedValue)
{
    // -- Given.
    // -- When.
    auto test = String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::IsNormalized);

    // -- Then.
    EXPECT_STREQ("P\xC3\xA4r Grindvik", test.asUTF8());
}

TEST_F(StringTests, StringWithUTF8_AUTF8StringWhenNormalizing_ContainsNormalizedValue)
{
    // -- Given.
    // -- When.
    auto test = String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::NeedsNormalizing);

    // -- Then.
    EXPECT_STREQ("Pa\xCC\x88r Grindvik", test.asUTF8());
}

TEST_F(StringTests, StringWithUTF16_StringCreatedFromUTF16String_ContainsCorrectValue)
{
    // -- Given.
    auto blob = Blob::withMemoryAndSize(stringTestsUTF16String, sizeof(stringTestsUTF16String));

    // -- When.
    auto test = String::stringWithUTF16(blob);

    // -- Then.
    EXPECT_STREQ(stringTestsUTF8String, test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountOfZero_ReturnsCorrectValue)
{
    // -- Given.
    auto byteCount = 0u;

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("0 bytes", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOneKB_ReturnsCorrectValue)
{
    // -- Given.
    auto byteCount = static_cast<count>(456);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("456 bytes", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOneMBButRoundValue_ReturnsCorrectValue)
{
    // -- Given.
    auto byteCount = static_cast<count>(370) * static_cast<count>(1024);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("370KB", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOneMB_ReturnsCorrectValue)
{
    // -- Given.
    auto byteCount = static_cast<count>(370) * static_cast<count>(1024) + static_cast<count>(368);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("370.36KB", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOneGBButRoundValue_ReturnsCorrectValue)
{
    // -- Given.
    auto byteCount = static_cast<count>(756) * static_cast<count>(1024) * static_cast<count>(1024);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("756MB", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOneGB_ReturnsCorrectValue)
{
    // -- Given.
    auto byteCount = static_cast<count>(756) * static_cast<count>(1024) * static_cast<count>(1024) +
            static_cast<count>(234) * static_cast<count>(1024);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("756.23MB", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOnePBButRoundValue_ReturnsCorrectValue)
{
    // -- Given.
    count byteCount = static_cast<count>(156) * static_cast<count>(1024) * static_cast<count>(1024) * static_cast<count>(1024);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("156GB", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInLessThanOnePB_ReturnsCorrectValue)
{
    // -- Given.
    count byteCount = static_cast<count>(156) * static_cast<count>(1024) * static_cast<count>(1024) *
            static_cast<count>(1024) + static_cast<count>(756) * static_cast<count>(1024) * static_cast<count>(1024);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("156.74GB", test.asUTF8());
}

TEST_F(StringTests, StringWithRepresentationOfByteCount_AByteCountInMoreThanOnePB_ReturnsCorrectValue)
{
    // -- Given.
    count byteCount = static_cast<count>(999) * static_cast<count>(1024) * static_cast<count>(1024) * static_cast<count>(1024) *
            static_cast<count>(1024) + static_cast<count>(56) * static_cast<count>(1024u) * static_cast<count>(1024) *
            static_cast<count>(1024) + static_cast<count>(756) * static_cast<count>(1024) * static_cast<count>(1024);

    // -- When.
    auto test = String::stringWithRepresentationOfByteCount(byteCount);

    // -- Then.
    EXPECT_STREQ("999.06PB", test.asUTF8());
}

TEST_F(StringTests, OperatorEqual_TwoEqualStrings_ReturnsTrue)
{
    // -- Given.
    String test(stringTestsUTF8String);
    String test2(stringTestsUTF8String);

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == test2);
}

TEST_F(StringTests, OperatorNotEqual_TwoUnequalStrings_ReturnsTrue)
{
    // -- Given.
    String test(stringTestsUTF8String);
    MutableString test2(stringTestsUTF8String);
    test2.append("more");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != test2);
}

TEST_F(StringTests, OperatorNotEqual_TwoEqualStrings_ReturnsFalse)
{
    // -- Given.
    String test(stringTestsUTF8String);
    String test2(stringTestsUTF8String);

    // -- When.
    // -- Then.
    EXPECT_FALSE(test != test2);
}

TEST_F(StringTests, OperatorEqual_TwoUnequalStrings_ReturnsFalse)
{
    // -- Given.
    String test(stringTestsUTF8String);
    MutableString test2(stringTestsUTF8String);
    test2.append("more");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == test2);
}

TEST_F(StringTests, OperatorEqual_AStringAndAnEqualCharacterPointer_ReturnsTrue)
{
    // -- Given.
    String test(stringTestsUTF8String);

    // -- When.
    // -- Then.
    EXPECT_TRUE(test == String(stringTestsUTF8String));
}

TEST_F(StringTests, OperatorEqual_AStringAndAnUnequalCharacterPointer_ReturnsFalse)
{
    // -- Given.
    MutableString test(stringTestsUTF8String);
    test.append("more");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test == String(stringTestsUTF8String));
}

TEST_F(StringTests, OperatorNotEqual_AStringAndAnEqualCharacterPointer_ReturnsFalse)
{
    // -- Given.
    String test(stringTestsUTF8String);

    // -- When.
    // -- Then.
    EXPECT_FALSE(test != String(stringTestsUTF8String));
}

TEST_F(StringTests, OperatorNotEqual_AStringAndAnUnequalCharacterPointer_ReturnsTrue)
{
    // -- Given.
    MutableString test(stringTestsUTF8String);
    test.append("more");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test != String(stringTestsUTF8String));
}

TEST_F(StringTests, OperatorSquareBrackets_AStringAndAnindex_ReturnsCorrectCharacter)
{
    // -- Given.
    String test(stringTestsUTF8String);

    // -- When.
    auto result = test[4];

    // -- Then.
    EXPECT_EQ(result, '$');
}

TEST_F(StringTests, OperatorSquareBrackets_AUTF8StringAndAnindex_ReturnsCorrectCharacter)
{
    // -- Given.
    String test("This ü is a test");

    // -- When.
    auto result = test[10];

    // -- Then.
    EXPECT_EQ(result, 'a');
}

TEST_F(StringTests, OperatorSquareBrackets_AStringAndAnindexOutOFBounds_Throws)
{
    // -- Given.
    String test(stringTestsUTF8String);

    // -- When.
    // -- Then.
    EXPECT_THROW(test[44352], AssertionFailed);
}

TEST_F(StringTests, Length_StringWithAValue_ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    String test(stringTestsUTF8String);

    // -- Then.
    EXPECT_EQ(test.length(), 95u);
}

TEST_F(StringTests, Length_UTF8StringWithAValue_ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    String test("Ἀριστüτέλης");

    // -- Then.
    EXPECT_EQ(test.length(), 11u);
}

TEST_F(StringTests, Length_StringWithEmptyString_ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    String test;

    // -- Then.
    EXPECT_EQ(test.length(), 0u);
}

TEST_F(StringTests, Length_StringWithNoValue_ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    String test("");

    // -- Then.
    EXPECT_EQ(test.length(), 0u);
}

TEST_F(StringTests, IsEmpty_StringWithAValue_ReturnFalse)
{
    // -- Given.
    // -- When.
    String test(stringTestsUTF8String);

    // -- Then.
    EXPECT_FALSE(test.isEmpty());
}

TEST_F(StringTests, IsEmpty_StringWithEmptyString_ReturnsTrue)
{
    // -- Given.
    // -- When.
    String test;

    // -- Then.
    EXPECT_TRUE(test.isEmpty());
}

TEST_F(StringTests, IsEmpty_StringWithNoValue_ReturnsTrue)
{
    // -- Given.
    // -- When.
    String test("");

    // -- Then.
    EXPECT_TRUE(test.isEmpty());
}

TEST_F(StringTests, sizeInBytesOfStringAsUTF8_UTF8StringWithAValue_ReturnsCorrectValue)
{
    // -- Given.
    // -- When.
    String test("Ἀριστοτέλης");

    // -- Then.
    EXPECT_EQ(test.sizeInBytesOfStringAsUTF8(), 23u);
}

TEST_F(StringTests, asUTF16_StringCreatedFromUTF8String_ReturnsCorrectValue)
{
    // -- Given.
    String test(stringTestsUTF8String);

    // -- When.
    auto result = test.asUTF16();

    // -- Then.
    EXPECT_EQ(test.length() * 2, result.size());
    EXPECT_EQ(sizeof(stringTestsUTF16String), result.size());
    EXPECT_EQ(0, ::memcmp(stringTestsUTF16String, result.data().get(), sizeof(stringTestsUTF16String)));
}

TEST_F(StringTests, asNormalizedString_AUTF8StringNotNormalized_ReturnsNormalizedValue)
{
    // -- Given.
    auto test = String::stringWithUTF8("Pär Grindvik", String::UTF8Flag::IsNormalized);

    // -- When.
    auto result = test.asNormalizedString();

    // -- Then.
    EXPECT_STREQ("P\xC3\xA4r Grindvik", test.asUTF8());
}

TEST_F(StringTests, asStringByAddingPercentEncoding_AStringWithCharactersToEscape_ReturnsEscapedString)
{
    // -- Given.
    String test("/Volumes/Test/Path !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ö.MP3");

    // -- When.
    auto result = test.asStringByAddingPercentEncoding();

    // -- Then.
    EXPECT_STREQ("/Volumes/Test/Path%20!%22%23$%25%26'()*+,-./0123456789:;%3C=%3E?@ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~o%CC%88.MP3", result.asUTF8());
}

TEST_F(StringTests, asStringByRemovingPercentEncoding_AStringWithCharactersEscaped_ReturnsDecodedString)
{
    // -- Given.
    String test("/Volumes/Test/Path%20!%22%23$%25%26'()*+,-./0123456789:;%3C=%3E?@ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~o%CC%88.MP3");

    // -- When.
    auto result = test.asStringByRemovingPercentEncoding();

    // -- Then.
    EXPECT_STREQ("/Volumes/Test/Path !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~ö.MP3", result.asUTF8());
}

TEST_F(StringTests, Append_AStringToAString_ReturnsCorrectValue)
{
    // -- Given.
    MutableString test1("Hello This ");
    String test2("Is A Test.");

    // -- When.
    test1.append(test2);

    // -- Then.
    EXPECT_STREQ("Hello This Is A Test.", test1.asUTF8());
}

TEST_F(StringTests, Append_AStringToACharacterPointer_ReturnsCorrectValue)
{
    // -- Given.
    MutableString test("Hello This ");

    // -- When.
    test.append("Is A Test.");

    // -- Then.
    EXPECT_STREQ("Hello This Is A Test.", test.asUTF8());
}

TEST_F(StringTests, SplitBySeparator_StringSeparatedByAnotherString_ReturnsCorrectValues)
{
    // -- Given.
    MutableString test("Hello, This Is, A, Test.");

    // -- When.
    auto result = test.splitBySeparator(", ");

    // -- Then.
    EXPECT_EQ(result.length(), 4u);
    EXPECT_EQ(result[0], "Hello");
    EXPECT_EQ(result[1], "This Is");
    EXPECT_EQ(result[2], "A");
    EXPECT_EQ(result[3], "Test.");
}

TEST_F(StringTests, SplitBySeparator_StringSeparatedByACharacter_ReturnsCorrectValues)
{
    // -- Given.
    MutableString test("Hello This Is A Test.");

    // -- When.
    auto result = test.splitBySeparator(' ');

    // -- Then.
    EXPECT_EQ(result.length(), 5u);
    EXPECT_EQ(result[0], "Hello");
    EXPECT_EQ(result[1], "This");
    EXPECT_EQ(result[2], "Is");
    EXPECT_EQ(result[3], "A");
    EXPECT_EQ(result[4], "Test.");
}

TEST_F(StringTests, SplitBySeparator_AnEmptyString_ReturnsNoValues)
{
    // -- Given.
    MutableString test;

    // -- When.
    auto result = test.splitBySeparator(", ");

    // -- Then.
    EXPECT_EQ(result.length(), 0u);
}

TEST_F(StringTests, SplitBySeparator_StringSeparatorsThatCouldCauseEmptyStrings_SkipsThePotentialEmptyStrings)
{
    // -- Given.
    MutableString test("Hello, This Is, , A, Test, ");

    // -- When.
    auto result = test.splitBySeparator(", ");

    // -- Then.
    EXPECT_EQ(result.length(), 4u);
    EXPECT_EQ(result[0], "Hello");
    EXPECT_EQ(result[1], "This Is");
    EXPECT_EQ(result[2], "A");
    EXPECT_EQ(result[3], "Test");
}

TEST_F(StringTests, SubString_FromAnIndex_ReturnsCorrectValue)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(6);

    // -- Then.
    EXPECT_EQ(result, "This Is A Test.");
}

TEST_F(StringTests, SubString_FromAnIndexWithUTFString_ReturnsCorrectValue)
{
    // -- Given.
    String test("Ἀριστοτέλης");

    // -- When.
    auto result = test.subString(6,10);

    // -- Then.
    EXPECT_EQ(result, "τέλη"_String);
}

TEST_F(StringTests, Substring_FromAnIndexAtTheEndOfTheString_ReturnsEmptyString)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(21);

    // -- Then.
    EXPECT_EQ(result, "");
}

TEST_F(StringTests, Substring_FromAnIndexPastTheEndOfTheString_ReturnsEmptyString)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(245);

    // -- Then.
    EXPECT_EQ(result, "");
}

TEST_F(StringTests, Substring_FromAnIndexToAnother_ReturnsCorrectValue)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(6, 10);

    // -- Then.
    EXPECT_EQ(result, "This");
}

TEST_F(StringTests, Substring_FromAnIndexToAnotherAtTheEndOfTheString_ReturnsCorrectValue)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(6, 21);

    // -- Then.
    EXPECT_EQ(result, "This Is A Test.");
}

TEST_F(StringTests, Substring_FromAnIndexToAnotherPastTheEndOfTheString_ReturnsCorrectValue)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(6, 234);

    // -- Then.
    EXPECT_EQ(result, "This Is A Test.");
}

TEST_F(StringTests, Substring_FromAnIndexAndToAnotherBothPastTheEndOfTheString_ReturnsEmptyString)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    auto result = test.subString(245, 255);

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "");
}

TEST_F(StringTests, Substring_FromAnIndexAndToAnotherInInverserOrderTheEndOfTheString_ReturnsEmptyString)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When/Then.
    EXPECT_THROW(test.subString(14, 2), NxA::AssertionFailed);
}

TEST_F(StringTests, LowerCaseString_StringWithUpperCaseCharacters_ReturnsOneWithLowerCaseUTFCharacters)
{
    // -- Given.
    String test("Mp3 grüßEN");

    // -- When.
    auto result = test.lowerCaseString();

    // -- Then.
    EXPECT_STREQ(result.asUTF8(), "mp3 grüßen");
}

TEST_F(StringTests, UpperCaseString_StringWithLowerCaseCharacters_ReturnsOneWithUpperCaseUTFCharacters)
{
    // -- Given.
    String test("Mp3 grüßENß");

    // -- When.
    auto result = test.upperCaseString();

    // -- Then.
    EXPECT_STREQ("MP3 GRÜSSENSS", result.asUTF8());
}

TEST_F(StringTests, HasPrefix_StringWithAGivenPrefix_ReturnsTrue)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.hasPrefix("Hello"));
}

TEST_F(StringTests, HasPrefix_StringWithoutAGivenPrefix_ReturnsFalse)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.hasPrefix("Hello2"));
}

TEST_F(StringTests, HasPrefix_StringWithAGivenPrefixAndAStringWithThatPrefix_ReturnsTrue)
{
    // -- Given.
    String test("Hello This Is A Test.");
    String test2("Hello");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.hasPrefix(test2));
}

TEST_F(StringTests, HasPrefixR_StringWithoutAGivenPrefixAndAStringWithThatPrefix_eturnsFalse)
{
    // -- Given.
    String test("Hello This Is A Test.");
    String test2("Hello2");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.hasPrefix(test2));
}

TEST_F(StringTests, HasPostfix_StringWithAGivenPostfix_ReturnsTrue)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.hasPostfix("Test."));
}

TEST_F(StringTests, HasPostfix_StringWithAGivenPostfixButAlsoSameStringElsewhere_ReturnsTrue)
{
    // -- Given.
    String test("Hello Test. This Is A Test.");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.hasPostfix("Test."));
}

TEST_F(StringTests, HasPostfix_StringWithoutAGivenPostfix_ReturnsFalse)
{
    // -- Given.
    String test("Hello This Is A Test.");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.hasPostfix("Test2"));
}

TEST_F(StringTests, HasPostfix_StringWithAGivenPostfixAndAStringWithThatPostfix_ReturnsTrue)
{
    // -- Given.
    String test("Hello This Is A Test.");
    String test2("Test.");

    // -- When.
    // -- Then.
    EXPECT_TRUE(test.hasPostfix(test2));
}

TEST_F(StringTests, HasPostfix_StringWithoutAGivenPostfixAndAStringWithThatPostfix_ReturnsFalse)
{
    // -- Given.
    String test("Hello This Is A Test.");
    String test2("Test2");

    // -- When.
    // -- Then.
    EXPECT_FALSE(test.hasPostfix(test2));
}

TEST_F(StringTests, MaybeIndexOfFirstOccurenceOf_StringWithoutAGivenSubStringAndStringWithThatSubString_ReturnsNothing)
{
    // -- Given.
    String test("Hello This Is A This Test.");
    String test2("Thit");

    // -- When.
    auto result = test.maybeIndexOfFirstOccurenceOf(test2);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(StringTests, MaybeIndexOfFirstOccurenceOf_StringWithAGivenSubStringAndStringWithThatSubString_ReturnsCorrectIndex)
{
    // -- Given.
    String test("Hellü This Is A This üest.");
    String test2("This");

    // -- When.
    auto result = test.maybeIndexOfFirstOccurenceOf(test2);

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_EQ(6u, *result);
}

TEST_F(StringTests, MaybeIndexOfLastOccurenceOf_StringWithoutAGivenSubStringAndStringWithThatSubString_ReturnsNothing)
{
    // -- Given.
    String test("Hello This Is A This Test.");
    String test2("Thit");

    // -- When.
    auto result = test.maybeIndexOfLastOccurenceOf(test2);

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(StringTests, MaybeIndexOfLastOccurenceOf_StringWithAGivenSubStringAndStringWithThatSubString_ReturnsCorrectIndex)
{
    // -- Given.
    String test("Helüo This Is A This Tüst.");
    String test2("This");

    // -- When.
    auto result = test.maybeIndexOfLastOccurenceOf(test2);

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_EQ(16u, *result);
}

TEST_F(StringTests, MaybeIndexOfFirstOccurenceOf_StringWithoutAGivenCharacter_ReturnsNothing)
{
    // -- Given.
    String test("Hello This Is A This Test.");

    // -- When.
    auto result = test.maybeIndexOfFirstOccurenceOf('k');

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(StringTests, MaybeIndexOfFirstOccurenceOf_StringWithAGivenCharacter_ReturnsCorrectIndex)
{
    // -- Given.
    String test("Hello Thüs Is A This Test.");

    // -- When.
    auto result = test.maybeIndexOfFirstOccurenceOf('I');

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_EQ(11u, *result);
}

TEST_F(StringTests, MaybeIndexOfLastOccurenceOf_StringWithoutAGivenSubStringAndCharacterWithThatSubString_ReturnsNothing)
{
    // -- Given.
    String test("Hello This Is A This Test.");

    // -- When.
    auto result = test.maybeIndexOfLastOccurenceOf('L');

    // -- Then.
    EXPECT_FALSE(result.isValid());
}

TEST_F(StringTests, MaybeIndexOfLastOccurenceOf_StringWithAGivenSubStringAndCharacterWithThatSubString_ReturnsCorrectIndex)
{
    // -- Given.
    String test("Hello Tüis Is A This Tüst.");

    // -- When.
    auto result = test.maybeIndexOfLastOccurenceOf('i');

    // -- Then.
    EXPECT_TRUE(result.isValid());
    EXPECT_EQ(18u, *result);
}

TEST_F(StringTests, Hash_SameStrings_SameHash)
{
    // -- Given.
    String testStr("Heülo");
    String testStr2("Heülo");

    // -- When.
    // -- Then.
    EXPECT_EQ(testStr2.hash(), testStr.hash());
}

TEST_F(StringTests, Hash_DifferentStrings_DifferentHash)
{
    // -- Given.
    String testStr("Hello2");
    String testStr2("Hello");

    // -- When.
    // -- Then.
    EXPECT_NE(testStr2.hash(), testStr.hash());
}

TEST_F(StringTests, IntegerValue_AStringWithAnInteger_ReturnsCorrectValue)
{
    // -- Given.
    String testStr("1345");

    // -- When.
    // -- Then.
    EXPECT_EQ(testStr.integerValue(), 1345);
}

TEST_F(StringTests, IntegerValue_AStringWithANegativeInteger_ReturnsCorrectValue)
{
    // -- Given.
    String testStr("-341345");

    // -- When.
    // -- Then.
    EXPECT_EQ(testStr.integerValue(), -341345);
}

TEST_F(StringTests, IntegerValue_AStringWithAInvalidInteger_ReturnsPartOfTheNumber)
{
    // -- Given.
    String testStr("-341d345");

    // -- When.
    // -- Then.
    EXPECT_EQ(testStr.integerValue(), -341);
}

TEST_F(StringTests, IntegerValue_AStringWithAInvalidString_ReturnsZero)
{
    // -- Given.
    String testStr("d-341d345");

    // -- When.
    // -- Then.
    EXPECT_EQ(0, testStr.integerValue());
}

TEST_F(StringTests, maybeUnsignedIntegerValueFromHexadecimal_AStringWithAnHexaDecimalNumber_ReturnsCorrectValue)
{
    // -- Given.
    String testStr("0xfF46D2");

    // -- When.
    auto maybeResult = testStr.maybeUnsignedIntegerValueFromHexadecimal();

    // -- Then.
    ASSERT_TRUE(maybeResult.isValid());
    EXPECT_EQ(*maybeResult, 16729810u);
}

TEST_F(StringTests,  maybeUnsignedIntegerValueFromHexadecimal_AStringWithAnHexaDecimalNumberButWithoutThePrefix_ReturnsCorrectValue)
{
    // -- Given.
    String testStr("fF46D2349e0037af");

    // -- When.
    auto maybeResult = testStr.maybeUnsignedIntegerValueFromHexadecimal();

    // -- Then.
    ASSERT_TRUE(maybeResult.isValid());
    EXPECT_EQ(*maybeResult, 18394620851472316335u);
}

TEST_F(StringTests, maybeUnsignedIntegerValueFromHexadecimal_AStringWithAnHexaDecimalNumberWithInvalidCharacters_ReturnsNothing)
{
    // -- Given.
    String testStr("0xfF4q6D2");

    // -- When.
    auto maybeResult = testStr.maybeUnsignedIntegerValueFromHexadecimal();

    // -- Then.
    ASSERT_FALSE(maybeResult.isValid());
}

TEST_F(StringTests,  maybeUnsignedIntegerValueFromHexadecimal_AStringWithAnHexaDecimalNumberTooLarge_ReturnsNothing)
{
    // -- Given.
    String testStr("f39839472738fF46D");

    // -- When.
    auto maybeResult = testStr.maybeUnsignedIntegerValueFromHexadecimal();

    // -- Then.
    ASSERT_FALSE(maybeResult.isValid());
}

TEST_F(StringTests, StringByReplacingOccurencesOfWith_AStringWithSomeCharactersToReplace_CorrectlyReplacesTheCharacters)
{
    // -- Given.
    String testStr("d-341d345");

    // -- When.
    auto result = testStr.stringByReplacingOccurencesOfWith("34", "A");

    // -- Then.
    EXPECT_STREQ("d-A1dA5", result.asUTF8());
}

TEST_F(StringTests, StringByReplacingOccurencesOfWith_AStringWithNoCharactersToReplace_ReturnsTheSameString)
{
    // -- Given.
    String testStr("d-341d345");

    // -- When.
    auto result = testStr.stringByReplacingOccurencesOfWith("8", "A");

    // -- Then.
    EXPECT_STREQ("d-341d345", result.asUTF8());
}

}
