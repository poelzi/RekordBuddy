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

#include <Base/Blob.hpp>
#include <Base/String.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

static const byte blobTestsTestData[] = {
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
    0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
    0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F, 0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, 0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
    0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, 0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
    0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, 0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
    0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, 0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF};

static const byte blobTestsTestHash[] = {0xD2, 0x76, 0xE4, 0xEF, 0x00, 0xA8, 0x28, 0xA9, 0xEA, 0x51, 0xB4, 0x83, 0xA4, 0xB6, 0xA8, 0x32};

static const String blobTestsTestBase64String("YXBwbGljYXRpb24vb2N0ZXQtc3RyZWFtAABTZXJhdG8gTWFya2VyczIAAQFBUUZEVDB4UFVn\nQUFBQUFFQVAvLy8wTlZSUUFBQUFBTkFBQUFBQ"
                                              "UFJQU13QUFBQUFBRU5WUlFBQUFBQU5BQUVB\nQXQxbUFNeUkKQUFBQUFFeFBUMUFBQUFBQUZRQUFBQUlpUndBQ01XYi8vLy8vQUNlcTRRQUFB\n"
                                              "RUpRVFV4UFEwc0FBQUFBQVFBQQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                                              "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
                                              "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                                              "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
                                              "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAAAAAAAAAAAAAAAAAAAAAA==\n");

static const byte blobTestsTestBinarySourceData[] = {
    0x61, 0x70, 0x70, 0x6C, 0x69, 0x63, 0x61, 0x74, 0x69, 0x6F, 0x6E, 0x2F, 0x6F, 0x63, 0x74, 0x65, 0x74, 0x2D, 0x73, 0x74, 0x72, 0x65, 0x61, 0x6D,
    0x00, 0x00, 0x53, 0x65, 0x72, 0x61, 0x74, 0x6F, 0x20, 0x4D, 0x61, 0x72, 0x6B, 0x65, 0x72, 0x73, 0x32, 0x00, 0x01, 0x01, 0x41, 0x51, 0x46, 0x44,
    0x54, 0x30, 0x78, 0x50, 0x55, 0x67, 0x41, 0x41, 0x41, 0x41, 0x41, 0x45, 0x41, 0x50, 0x2F, 0x2F, 0x2F, 0x30, 0x4E, 0x56, 0x52, 0x51, 0x41, 0x41,
    0x41, 0x41, 0x41, 0x4E, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x49, 0x41, 0x4D, 0x77, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x45, 0x4E, 0x56,
    0x52, 0x51, 0x41, 0x41, 0x41, 0x41, 0x41, 0x4E, 0x41, 0x41, 0x45, 0x41, 0x41, 0x74, 0x31, 0x6D, 0x41, 0x4D, 0x79, 0x49, 0x0A, 0x41, 0x41, 0x41,
    0x41, 0x41, 0x45, 0x78, 0x50, 0x54, 0x31, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x46, 0x51, 0x41, 0x41, 0x41, 0x41, 0x49, 0x69, 0x52, 0x77, 0x41,
    0x43, 0x4D, 0x57, 0x62, 0x2F, 0x2F, 0x2F, 0x2F, 0x2F, 0x41, 0x43, 0x65, 0x71, 0x34, 0x51, 0x41, 0x41, 0x41, 0x45, 0x4A, 0x51, 0x54, 0x55, 0x78,
    0x50, 0x51, 0x30, 0x73, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x51, 0x41, 0x41, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00};

class BlobTests : public NxA::Test
{

};

TEST_F(BlobTests, Constructor_NoArgument_BlobCreatedCorrectly)
{
    // -- Given.
    // -- When.
    Blob test;

    // -- Then.
    EXPECT_EQ(0u, test.size());
}

TEST_F(BlobTests, Constructor_ArgumentsWithMemoryAndSize_BlobCreatedCorrectly)
{
    // -- Given.
    // -- When.
    Blob test(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, BlobWithCapacity_AGivenSize_BlobCreatedCorrectly)
{
    // -- Given.
    constexpr count testSize = 32;

    // -- When.
    auto test = MutableBlob::withCapacity(testSize);

    // -- Then.
    EXPECT_EQ(testSize, test.size());
    auto data = test.data();
    for (count i = 0; i < testSize; ++i) {
        EXPECT_EQ(0, data[i]);
    }
}

TEST_F(BlobTests, BlobWithMemoryAndSize_SomeDataInMemory_BlobCreatedCorrectly)
{
    // -- Given.
    // -- When.
    auto test = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, withBase64String_ABase64String_BlobCreatedCorrectly)
{
    // -- Given.
    // -- When.
    auto test = Blob::withBase64String(blobTestsTestBase64String);

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestBinarySourceData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestBinarySourceData, sizeof(blobTestsTestBinarySourceData)));
}

TEST_F(BlobTests, BlobWith_BlobCreatedFromAnotherBlob_BlobCreatedCorrectly)
{
    // -- Given.
    auto source = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    auto test = Blob(source);

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, base64String_hashFor_ReturnsTheCorrectHash)
{
    // -- Given.
    // -- When.
    auto result = Blob::hashFor(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestHash), result.size());
    EXPECT_EQ(0, ::memcmp(result.data().get(), blobTestsTestHash, sizeof(blobTestsTestHash)));
}

TEST_F(BlobTests, base64String_base64StringFor_ReturnsTheCorrectBase64String)
{
    // -- Given.
    // -- When.
    auto result = Blob::base64StringFor(blobTestsTestBinarySourceData, sizeof(blobTestsTestBinarySourceData));

    // -- Then.
    EXPECT_EQ(blobTestsTestBase64String.length(), result.length());
    EXPECT_EQ(blobTestsTestBase64String, result);
}

TEST_F(BlobTests, OperatorSquareBrackets_BlobWithDataInIt_ReturnsCorrectData)
{
    // -- Given.
    // -- When.
    auto test = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, OperatorSquareBrackets_ConstantBlobWithDataInIt_ReturnsCorrectData)
{
    // -- Given.
    // -- When.
    auto const test = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, OperatorSquareBrackets_OutOfBoundsAccess_ThrowsAnException)
{
    // -- Given.
    // -- When.
    auto test = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_THROW(test[sizeof(blobTestsTestData)], NxA::AssertionFailed);
    EXPECT_THROW(test[sizeof(blobTestsTestData) + 46], NxA::AssertionFailed);
}

TEST_F(BlobTests, OperatorSquareBrackets_OutOfBoundsAccessOnConstantBlob_ThrowsAnException)
{
    // -- Given.
    // -- When.
    auto const test = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- Then.
    EXPECT_THROW(test[sizeof(blobTestsTestData)], NxA::AssertionFailed);
    EXPECT_THROW(test[sizeof(blobTestsTestData) + 46], NxA::AssertionFailed);
}

TEST_F(BlobTests, Data_BlobIsEmpty_ThrowsAnException)
{
    // -- Given.
    // -- When.
    Blob test;

    // -- Then.
    EXPECT_THROW(test.data(), NxA::AssertionFailed);
}

TEST_F(BlobTests, Data_ConstantBlobIsEmpty_ThrowsAnException)
{
    // -- Given.
    // -- When.
    const Blob test;

    // -- Then.
    EXPECT_THROW(test.data(), NxA::AssertionFailed);
}

TEST_F(BlobTests, OperatorEqual_TwoEqualBlobs_ReturnsTrue)
{
    // -- Given.
    auto source = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    auto test = Blob(source);

    // -- Then.
    EXPECT_TRUE(source == test);
}

TEST_F(BlobTests, OperatorEqual_TwoUnequalBlobs_ReturnsFalse)
{
    // -- Given.
    auto source = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    Blob test;

    // -- Then.
    EXPECT_FALSE(source == test);
}

TEST_F(BlobTests, FillWithZeros_ABlobWithContent_FillsTheBlobWithZeros)
{
    // -- Given.
    auto test = MutableBlob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    test.fillWithZeros();

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    auto data = test.data();
    for (count i = 0; i < sizeof(blobTestsTestData); ++i) {
        EXPECT_EQ(0, data[i]);
    }
}

TEST_F(BlobTests, Hash_ABlobWithContent_ReturnsTheCorrectHashValue)
{
    // -- Given.
    auto test = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    auto result = test.hash();

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestHash), result.size());
    EXPECT_EQ(0, ::memcmp(result.data().get(), blobTestsTestHash, sizeof(blobTestsTestHash)));
}

TEST_F(BlobTests, base64String_ABlobWithBinaryData_ReturnsTheCorrectBase64String)
{
    // -- Given.
    auto test = Blob::withMemoryAndSize(blobTestsTestBinarySourceData, sizeof(blobTestsTestBinarySourceData));

    // -- When.
    auto result = test.base64String();

    // -- Then.
    EXPECT_EQ(blobTestsTestBase64String.length(), result.length());
    EXPECT_EQ(blobTestsTestBase64String, result);
}

TEST_F(BlobTests, asStringsFromZeroSeparatedData_ABlobWithZeroSeperatedStrings_ReturnsTheStrings)
{
    // -- Given.
    static const byte sourceData[] = {
            't', 'e', 's', 't', 0, 'd', 'o', 'e', 's', 0, 't', 'h', 'i', 's', 0, 'w', 'o', 'r', 'k', '?', 0
    };
    auto blob = Blob::withMemoryAndSize(sourceData, sizeof(sourceData));

    // -- When.
    auto results = blob.asStringsFromZeroSeparatedData();

    // -- Then.
    ASSERT_EQ(results.length(), 4u);
    EXPECT_STREQ(results[0].asUTF8(), "test");
    EXPECT_STREQ(results[1].asUTF8(), "does");
    EXPECT_STREQ(results[2].asUTF8(), "this");
    EXPECT_STREQ(results[3].asUTF8(), "work?");
}

TEST_F(BlobTests, asStringsFromZeroSeparatedData_ABlobWithZeroSeperatedStrings_ButNotTheLastOne_ReturnsTheFirstStrings)
{
    // -- Given.
    static const byte sourceData[] = {
            't', 'e', 's', 't', 0, 'd', 'o', 'e', 's', 0, 't', 'h', 'i', 's', 0, 'w', 'o', 'r', 'k', '?'
    };
    auto blob = Blob::withMemoryAndSize(sourceData, sizeof(sourceData));

    // -- When.
    auto results = blob.asStringsFromZeroSeparatedData();

    // -- Then.
    ASSERT_EQ(results.length(), 3u);
    EXPECT_STREQ(results[0].asUTF8(), "test");
    EXPECT_STREQ(results[1].asUTF8(), "does");
    EXPECT_STREQ(results[2].asUTF8(), "this");
}

TEST_F(BlobTests, Append_AnEmptyBlobAndBlobWithContent_AppendTheContentCorrectly)
{
    // -- Given.
    MutableBlob test;
    auto test2 = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    test.append(test2);

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, CopyConstructor_ABlobWithBinaryData_ReturnsACorrectCopy)
{
    // -- Given.
    auto test = MutableBlob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    auto mutableCopy = MutableBlob(test);

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), mutableCopy.size());
    EXPECT_EQ(0, ::memcmp(mutableCopy.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, Append_TwoBlobsWithContent_AppendTheContentCorrectly)
{
    // -- Given.
    auto test = MutableBlob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));
    auto test2 = Blob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));

    // -- When.
    test.append(test2);

    // -- Then.
    EXPECT_EQ(2 * sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
    EXPECT_EQ(0, ::memcmp(test.data().get() + sizeof(blobTestsTestData), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, Append_ABlobWithContentAndEmptyBlob_AppendTheContentCorrectly)
{
    // -- Given.
    auto test = MutableBlob::withMemoryAndSize(blobTestsTestData, sizeof(blobTestsTestData));
    Blob test2;

    // -- When.
    test.append(test2);

    // -- Then.
    EXPECT_EQ(sizeof(blobTestsTestData), test.size());
    EXPECT_EQ(0, ::memcmp(test.data().get(), blobTestsTestData, sizeof(blobTestsTestData)));
}

TEST_F(BlobTests, Append_TwoEmptyBlobs_LeavesTheBlobEmpty)
{
    // -- Given.
    MutableBlob test;
    Blob test2;

    // -- When.
    test.append(test2);

    // -- Then.
    EXPECT_EQ(0u, test.size());
}

TEST_F(BlobTests, AppendWithStringTermination_AnEmptyBlobAndAString_AppendTheStringCorrectly)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    test.appendWithStringTermination("Hello");

    // -- Then.
    EXPECT_EQ(6u, test.size());
    auto data = test.data();
    EXPECT_EQ('H', data[0]);
    EXPECT_EQ('e', data[1]);
    EXPECT_EQ('l', data[2]);
    EXPECT_EQ('l', data[3]);
    EXPECT_EQ('o', data[4]);
    EXPECT_EQ(0, data[5]);
}

TEST_F(BlobTests, AppendWithoutStringTermination_AnEmptyBlobAndAString_AppendTheStringCorrectly)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    test.appendWithoutStringTermination("Hello");

    // -- Then.
    EXPECT_EQ(5u, test.size());
    auto data = test.data();
    EXPECT_EQ('H', data[0]);
    EXPECT_EQ('e', data[1]);
    EXPECT_EQ('l', data[2]);
    EXPECT_EQ('l', data[3]);
    EXPECT_EQ('o', data[4]);
}

TEST_F(BlobTests, AppendWithStringTermination_AnEmptyBlobAndAnEmptyString_OnlyAppendsAStringTerminatorCharacter)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    test.appendWithStringTermination("");

    // -- Then.
    EXPECT_EQ(1u, test.size());
    auto data = test.data();
    EXPECT_EQ(data[0], '\0');
}

TEST_F(BlobTests, AppendWithoutStringTermination_AnEmptyBlobAndAnEmptyString_OnlyAppendsAStringTerminatorCharacter)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    test.appendWithoutStringTermination("");

    // -- Then.
    EXPECT_EQ(0u, test.size());
}

TEST_F(BlobTests, Append_AnEmptyBlobAndACharacter_AppendsTheCharacter)
{
    // -- Given.
    MutableBlob test;

    // -- When.
    test.append('G');

    // -- Then.
    EXPECT_EQ(1u, test.size());
    auto data = test.data();
    EXPECT_EQ(data[0], 'G');
}

TEST_F(BlobTests, Remove_ABlobWithSomeCharacters_RemovesTheCorrectCharacters)
{
    // -- Given.
    MutableBlob test;
    test.appendWithoutStringTermination("Hello");

    // -- When.
    test.remove('l');

    // -- Then.
    EXPECT_EQ(3u, test.size());
    auto data = test.data();
    EXPECT_EQ('H', data[0]);
    EXPECT_EQ('e', data[1]);
    EXPECT_EQ('o', data[2]);
}

}
