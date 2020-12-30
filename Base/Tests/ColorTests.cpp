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

#include <Base/Color.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class ColorTests : public NxA::Test
{

};

TEST_F(ColorTests, ConstructorWithSeparateComponents_SomeInitialValues_ReturnsObjectHasCorrectValues)
{
    // -- Given.
    // -- When.
    auto color = Color(1, 2, 3, 4);

    // -- Then.
    EXPECT_EQ(1, color.red());
    EXPECT_EQ(2, color.green());
    EXPECT_EQ(3, color.blue());
    EXPECT_EQ(4, color.alpha());
}

TEST_F(ColorTests, ConstructorWithRGBAValue_AnInitialValue_ReturnsObjectHasCorrectValues)
{
    // -- Given.
    // -- When.
    auto color = Color(0x01020304);

    // -- Then.
    EXPECT_EQ(1, color.red());
    EXPECT_EQ(2, color.green());
    EXPECT_EQ(3, color.blue());
    EXPECT_EQ(4, color.alpha());
}

TEST_F(ColorTests, asRGB_AColor_ReturnsACorrectRGBValue)
{
    // -- Given.
    auto color = Color(1, 2, 3, 4);

    // -- When.
    auto asRGB = color.asRGB();

    // -- Then.
    EXPECT_EQ(0x00010203u, asRGB);
}

TEST_F(ColorTests, asRGBA_AColor_ReturnsACorrectRGBAValue)
{
    // -- Given.
    auto color = Color(1, 2, 3, 4);

    // -- When.
    auto asRGBA = color.asRGBA();

    // -- Then.
    EXPECT_EQ(0x01020304u, asRGBA);
}

TEST_F(ColorTests, operatorEqual_TwoEqualColors_ReturnsTrue)
{
    // -- Given.
    auto color1 = Color(1, 2, 3, 4);
    auto color2 = Color(1, 2, 3, 4);

    // -- When.
    auto isEqual = (color1 == color2);

    // -- Then.
    EXPECT_TRUE(isEqual);
}

TEST_F(ColorTests, operatorEqual_TwoUnequalColors_ReturnsFalse)
{
    // -- Given.
    auto color1 = Color(1, 2, 3, 4);
    auto color2 = Color(1, 1, 3, 4);

    // -- When.
    auto isEqual = (color1 == color2);

    // -- Then.
    EXPECT_FALSE(isEqual);
}

TEST_F(ColorTests, operatorLessThan_TwoColorsFirstLessThanSecond_ReturnsTrue)
{
    // -- Given.
    auto color1 = Color(1, 1, 3, 4);
    auto color2 = Color(1, 2, 3, 4);

    // -- When.
    auto isEqual = (color1 < color2);

    // -- Then.
    EXPECT_TRUE(isEqual);
}

TEST_F(ColorTests, operatorLessThan_TwoColorsFirstMoreThanSecond_ReturnsFalse)
{
    // -- Given.
    auto color1 = Color(2, 2, 3, 7);
    auto color2 = Color(1, 2, 3, 5);

    // -- When.
    auto isEqual = (color1 < color2);

    // -- Then.
    EXPECT_FALSE(isEqual);
}

TEST_F(ColorTests, operatorLessThan_TwoEqualColors_ReturnsFalse)
{
    // -- Given.
    auto color1 = Color(1, 2, 3, 4);
    auto color2 = Color(1, 2, 3, 4);

    // -- When.
    auto isEqual = (color1 < color2);

    // -- Then.
    EXPECT_FALSE(isEqual);
}

TEST_F(ColorTests, hash_TwoEqualColors_ReturnsSameHash)
{
    // -- Given.
    auto color1 = Color(1, 2, 3, 4);
    auto color2 = Color(1, 2, 3, 4);

    // -- When.
    auto hash1 = color1.hash();
    auto hash2 = color2.hash();

    // -- Then.
    EXPECT_EQ(hash1, hash2);
}

TEST_F(ColorTests, hash_TwoEqualColors_ReturnsDifferentHash)
{
    // -- Given.
    auto color1 = Color(1, 2, 3, 4);
    auto color2 = Color(1, 2, 1, 4);

    // -- When.
    auto hash1 = color1.hash();
    auto hash2 = color2.hash();

    // -- Then.
    EXPECT_NE(hash1, hash2);
}

}
