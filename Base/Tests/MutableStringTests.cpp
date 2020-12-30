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
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

class MutableStringTests : public NxA::Test
{

};

TEST_F(MutableStringTests, StringByReplacingOccurencesOfWith_AStringWithSomeCharactersToReplace_CorrectlyReplacesTheCharacters)
{
    // -- Given.
    MutableString testStr("d-341d345");

    // -- When.
    auto result = testStr.stringByReplacingOccurencesOfWith("34", "A");

    // -- Then.
    EXPECT_STREQ("d-A1dA5", result.asUTF8());
}

TEST_F(MutableStringTests, StringByReplacingOccurencesOfWith_AStringWithNoCharactersToReplace_ReturnsTheSameString)
{
    // -- Given.
    MutableString testStr("d-341d345");

    // -- When.
    auto result = testStr.stringByReplacingOccurencesOfWith("8", "A");

    // -- Then.
    EXPECT_STREQ("d-341d345", result.asUTF8());
}

TEST_F(MutableStringTests, replaceOccurenceOfWith_AStringToReplace_StringIsReplacedCorrectly)
{
    // -- Given.
    MutableString test("This is a tist.");

    // -- When.
    test.replaceOccurenceOfWith("is", "e*s");

    // -- Then.
    EXPECT_STREQ("The*s e*s a te*st.", test.asUTF8());
}
    
TEST_F(MutableStringTests, ReplaceOccurencesOfWith_AStringWithSomeCharactersToReplace_CorrectlyReplacesTheCharacters)
{
    // -- Given.
    MutableString testStr("d-341d345");

    // -- When.
    testStr.replaceOccurenceOfWith("34", "A");

    // -- Then.
    EXPECT_STREQ("d-A1dA5", testStr.asUTF8());
}

TEST_F(MutableStringTests, ReplaceOccurencesOfWith_AStringWithNoCharactersToReplace_StringIsUnchanged)
{
    // -- Given.
    MutableString testStr("d-341d345");

    // -- When.
    testStr.replaceOccurenceOfWith("8", "A");

    // -- Then.
    EXPECT_STREQ("d-341d345", testStr.asUTF8());
}

}
