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

#include <Base/Platform.hpp>
#include <Base/Test.hpp>
#include <Base/Blob.hpp>

using namespace testing;

namespace NxA {

class PlatformTests : public NxA::Test
{

};

TEST_F(PlatformTests, fromOrToLittleEndianUInteger64_AValue_ValueIsConvertedCorrectlyOrNotDependingOnEndianNess)
{
    // -- Given.
    // -- When.
    auto result = Platform::fromOrToLittleEndianUInteger64(0x1122334455667788u);

    // -- Then.
    if (Platform::endianOrder() == Platform::Endian::Big) {
        EXPECT_EQ(0x8877665544332211u, result);
    }
    else if (Platform::endianOrder() == Platform::Endian::Little) {
        EXPECT_EQ(0x1122334455667788u, result);
    }
    else {
        NXA_ALOG("Unsupported Platform.");
    }
}

TEST_F(PlatformTests, fromOrToLittleEndianUInteger32_AValue_ValueIsConvertedCorrectlyOrNotDependingOnEndianNess)
{
    // -- Given.
    // -- When.
    auto result = Platform::fromOrToLittleEndianUInteger32(0x11223344u);

    // -- Then.
    if (Platform::endianOrder() == Platform::Endian::Big) {
        EXPECT_EQ(0x44332211u, result);
    }
    else if (Platform::endianOrder() == Platform::Endian::Little) {
        EXPECT_EQ(0x11223344u, result);
    }
    else {
        NXA_ALOG("Unsupported Platform.");
    }
}

TEST_F(PlatformTests, fromOrToLittleEndianUInteger16_AValue_ValueIsConvertedCorrectlyOrNotDependingOnEndianNess)
{
    // -- Given.
    // -- When.
    auto result = Platform::fromOrToLittleEndianUInteger16(0x1122u);

    // -- Then.
    if (Platform::endianOrder() == Platform::Endian::Big) {
        EXPECT_EQ(0x2211u, result);
    }
    else if (Platform::endianOrder() == Platform::Endian::Little) {
        EXPECT_EQ(0x1122u, result);
    }
    else {
        NXA_ALOG("Unsupported Platform.");
    }
}

TEST_F(PlatformTests, fromOrToBigEndianUInteger64_AValue_ValueIsConvertedCorrectlyOrNotDependingOnEndianNess)
{
    // -- Given.
    // -- When.
    auto result = Platform::fromOrToBigEndianUInteger64(0x1122334455667788u);

    // -- Then.
    if (Platform::endianOrder() == Platform::Endian::Big) {
        EXPECT_EQ(0x1122334455667788u, result);
    }
    else if (Platform::endianOrder() == Platform::Endian::Little) {
        EXPECT_EQ(0x8877665544332211u, result);
    }
    else {
        NXA_ALOG("Unsupported Platform.");
    }
}

TEST_F(PlatformTests, fromOrToBigEndianUInteger32_AValue_ValueIsConvertedCorrectlyOrNotDependingOnEndianNess)
{
    // -- Given.
    // -- When.
    auto result = Platform::fromOrToBigEndianUInteger32(0x11223344u);

    // -- Then.
    if (Platform::endianOrder() == Platform::Endian::Big) {
        EXPECT_EQ(0x11223344u, result);
    }
    else if (Platform::endianOrder() == Platform::Endian::Little) {
        EXPECT_EQ(0x44332211u, result);
    }
    else {
        NXA_ALOG("Unsupported Platform.");
    }
}

TEST_F(PlatformTests, fromOrToBigEndianUInteger16_AValue_ValueIsConvertedCorrectlyOrNotDependingOnEndianNess)
{
    // -- Given.
    // -- When.
    auto result = Platform::fromOrToBigEndianUInteger16(0x1122u);

    // -- Then.
    if (Platform::endianOrder() == Platform::Endian::Big) {
        EXPECT_EQ(0x1122u, result);
    }
    else if (Platform::endianOrder() == Platform::Endian::Little) {
        EXPECT_EQ(0x2211u, result);
    }
    else {
        NXA_ALOG("Unsupported Platform.");
    }
}

TEST_F(PlatformTests, bigEndianFloatValueAt_AnFloat_ValueIsReturnedCorrectly)
{
    // -- Given.
    byte value[] = { 0x41, 0xBE, 0x49, 0xBA };

    // -- When.
    auto result = Platform::bigEndianFloatValueAt(value);

    // -- Then.
    EXPECT_FLOAT_EQ(23.7859993, result);
}

TEST_F(PlatformTests, bigEndianUInteger64ValueAt_AnInteger64_ValueIsReturnedCorrectly)
{
    // -- Given.
    byte value[] = { 0XFF, 0XDE, 0XAD, 0XBE, 0XEF, 0X87, 0X23, 0XFF };

    // -- When.
    auto result = Platform::bigEndianUInteger64ValueAt(value);

    // -- Then.
    EXPECT_EQ(0xFFDEADBEEF8723FF, result);
}

TEST_F(PlatformTests, bigEndianUInteger32ValueAt_AnInteger64_ValueIsReturnedCorrectly)
{
    // -- Given.
    byte value[] = { 0XFF, 0XDE, 0XAD, 0XBE };

    // -- When.
    auto result = Platform::bigEndianUInteger32ValueAt(value);

    // -- Then.
    EXPECT_EQ(0xFFDEADBE, result);
}

TEST_F(PlatformTests, bigEndianUInteger64ValueAt_AnInteger16_ValueIsReturnedCorrectly)
{
    // -- Given.
    byte value[] = { 0XFF, 0XDE };

    // -- When.
    auto result = Platform::bigEndianUInteger16ValueAt(value);

    // -- Then.
    EXPECT_EQ(0xFFDE, result);
}

TEST_F(PlatformTests, writeBigEndianFloatValueAt_AnFloat_CorrectValuesAreWritten)
{
    // -- Given.
    byte result[] = { 0x00, 0x00, 0x00, 0x00, 0x23 };

    // -- When.
    Platform::writeBigEndianFloatValueAt(23.786, result);

    // -- Then.
    EXPECT_EQ(0x41, result[0]);
    EXPECT_EQ(0xBE, result[1]);
    EXPECT_EQ(0x49, result[2]);
    EXPECT_EQ(0xBA, result[3]);
    EXPECT_EQ(0x23, result[4]);
}

TEST_F(PlatformTests, writeBigEndianInteger64tValueAt_AnInteger64_CorrectValuesAreWritten)
{
    // -- Given.
    byte result[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x23 };

    // -- When.
    Platform::writeBigEndianUInteger64ValueAt(0xFFDEADBEEF8725FF, result);

    // -- Then.
    EXPECT_EQ(0xFF, result[0]);
    EXPECT_EQ(0xDE, result[1]);
    EXPECT_EQ(0xAD, result[2]);
    EXPECT_EQ(0xBE, result[3]);
    EXPECT_EQ(0xEF, result[4]);
    EXPECT_EQ(0x87, result[5]);
    EXPECT_EQ(0x25, result[6]);
    EXPECT_EQ(0xFF, result[7]);
    EXPECT_EQ(0x23, result[8]);
}

TEST_F(PlatformTests, writeBigEndianInteger32tValueAt_AnInteger32_CorrectValuesAreWritten)
{
    // -- Given.
    byte result[] = { 0x00, 0x00, 0x00, 0x00, 0x23 };

    // -- When.
    Platform::writeBigEndianUInteger32ValueAt(0xFFDEADBE, result);

    // -- Then.
    EXPECT_EQ(0xFF, result[0]);
    EXPECT_EQ(0xDE, result[1]);
    EXPECT_EQ(0xAD, result[2]);
    EXPECT_EQ(0xBE, result[3]);
    EXPECT_EQ(0x23, result[4]);
}

TEST_F(PlatformTests, writeBigEndianInteger16tValueAt_AnInteger16_CorrectValuesAreWritten)
{
    // -- Given.
    byte result[] = { 0x00, 0x00, 0x23 };

    // -- When.
    Platform::writeBigEndianUInteger16ValueAt(0xFFDE, result);

    // -- Then.
    EXPECT_EQ(0xFF, result[0]);
    EXPECT_EQ(0xDE, result[1]);
    EXPECT_EQ(0x23, result[2]);
}

TEST_F(PlatformTests, convertEndiannessOfUInteger16From_AnInteger16InABlob_CorrectValueIsReturned)
{
    // -- Given.
    MutableBlob test;
    test.append(0x23);
    test.append(0xDC);

    // -- When.
    auto result = Platform::convertEndiannessOfUInteger16From(test);

    // -- Then.
    EXPECT_EQ(2u, result.size());
    EXPECT_EQ(0xDC, result[0]);
    EXPECT_EQ(0x23, result[1]);
}

}
