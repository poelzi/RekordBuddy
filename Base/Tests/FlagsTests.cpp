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

#include <Base/Flags.hpp>
#include <Base/Test.hpp>

using namespace testing;

namespace NxA {

enum class FlagsTestsTestFlag {
    OneOfThese,
    SomeOthers,
    AndEvenMore,

     // -- This is required for static assertions.
    LastFlag
};
    
class FlagsTests : public NxA::Test
{

};

TEST_F(FlagsTests, BoolOperator_EmptyFlags_ReturnsFalse)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags;

    // -- When.
    // -- Then.
    EXPECT_FALSE(flags);
}

TEST_F(FlagsTests, BoolOperator_FlagsWithOneSet_ReturnsTrue)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::OneOfThese);

    // -- When.
    // -- Then.
    EXPECT_TRUE(flags);
}

TEST_F(FlagsTests, BoolOperator_FlagsWithManySet_ReturnsTrue)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::OneOfThese);
    flags.set(FlagsTestsTestFlag::SomeOthers);

    // -- When.
    // -- Then.
    EXPECT_TRUE(flags);
}

TEST_F(FlagsTests, HasAny_FlagsWithNothingSet_ReturnsFalse)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags;

    // -- When.
    // -- Then.
    EXPECT_FALSE(flags.has(FlagsTestsTestFlag::AndEvenMore));
}

TEST_F(FlagsTests, HasAny_FlagsWithOneSet_ReturnsTrue)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);

    // -- When.
    // -- Then.
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::AndEvenMore));
}

TEST_F(FlagsTests, HasAny_FlagsWithAnotherSet_ReturnsFalse)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::OneOfThese);

    // -- When.
    // -- Then.
    EXPECT_FALSE(flags.has(FlagsTestsTestFlag::AndEvenMore));
}

TEST_F(FlagsTests, HasAny_FlagsWithMultipleSet_ReturnsTrue)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);
    flags.set(FlagsTestsTestFlag::OneOfThese);

    // -- When.
    // -- Then.
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::AndEvenMore));
}

TEST_F(FlagsTests, HasAll_FlagsWithMultipleSet_ReturnsTrue)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);
    flags.set(FlagsTestsTestFlag::SomeOthers);

    // -- When.
    // -- Then.
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::AndEvenMore));
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::SomeOthers));
}

TEST_F(FlagsTests, ClearAll_FlagsWithMultipleSet_ReturnsEmptyFlags)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);
    flags.set(FlagsTestsTestFlag::SomeOthers);

    // -- When.
    flags.clearAll();

    // -- Then.
    EXPECT_FALSE(flags);
}

TEST_F(FlagsTests, Set_FlagsWithOneSettingAnother_ReturnsFlagsWithBothSet)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);

    // -- When.
    flags.set(FlagsTestsTestFlag::SomeOthers);

    // -- Then.
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::AndEvenMore));
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::SomeOthers));
}

TEST_F(FlagsTests, AndAlso_FlagsWithOneAndAlsoAnotherViaEnum_ReturnsFlagsWithBothSet)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);

    // -- When.
    auto result = flags.andAlso(FlagsTestsTestFlag::SomeOthers);

    // -- Then.
    EXPECT_TRUE(result.has(FlagsTestsTestFlag::AndEvenMore));
    EXPECT_TRUE(result.has(FlagsTestsTestFlag::SomeOthers));
}

TEST_F(FlagsTests, AndAlso_FlagsWithOneAndAlsoAnother_ReturnsFlagsWithBothSet)
{
    // -- Given.
    Flags<FlagsTestsTestFlag> flags(FlagsTestsTestFlag::AndEvenMore);
    Flags<FlagsTestsTestFlag> otherFlags(FlagsTestsTestFlag::SomeOthers);

    // -- When.
    auto result = flags.andAlso(otherFlags);

    // -- Then.
    EXPECT_TRUE(result.has(FlagsTestsTestFlag::AndEvenMore));
    EXPECT_TRUE(result.has(FlagsTestsTestFlag::SomeOthers));
}

TEST_F(FlagsTests, Clear_FlagsWithTwoSet_ReturnsFlagsWithOnlyOneSet)
{
    // -- Given.
    auto flags = Flags<FlagsTestsTestFlag>(FlagsTestsTestFlag::AndEvenMore).andAlso(FlagsTestsTestFlag::SomeOthers);

    // -- When.
    flags.clear(FlagsTestsTestFlag::AndEvenMore);

    // -- Then.
    EXPECT_FALSE(flags.has(FlagsTestsTestFlag::AndEvenMore));
    EXPECT_TRUE(flags.has(FlagsTestsTestFlag::SomeOthers));
}

TEST_F(FlagsTests, ClearAll_FlagsWithTwoSet_ReturnsFlagsWithNothingSet)
{
    // -- Given.
    auto flags = Flags<FlagsTestsTestFlag>(FlagsTestsTestFlag::AndEvenMore).andAlso(FlagsTestsTestFlag::SomeOthers);

    // -- When.
    flags.clearAll();

    // -- Then.
    EXPECT_FALSE(flags.has(FlagsTestsTestFlag::AndEvenMore));
    EXPECT_FALSE(flags.has(FlagsTestsTestFlag::SomeOthers));
}

}
