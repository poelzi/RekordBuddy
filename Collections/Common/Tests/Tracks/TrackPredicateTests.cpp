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

#include <Base/Test.hpp>

#include <CommonCollection/Tracks/TrackPredicate.hpp>

using namespace testing;

namespace NxA { namespace Common {

class TrackPredicateTests : public NxA::Test
{

};

TEST_F(TrackPredicateTests, AComparisonPredicate_comparisonWith_ReturnsCorrectPredicate)
{
    // -- Given.
    auto predicate = TrackPredicate::comparisonWith(Common::Property::TypeID::Color, TrackPredicate::ComparisonOperator::EndsWith, "hello");

    // -- When.
    auto result = predicate.asBlob();

    // -- Then.
    EXPECT_EQ(13u, result.size());
    auto internal = reinterpret_cast<const TrackPredicate::ComparisonDataHeader*>(result.data().get());
    EXPECT_EQ(TrackPredicate::Type::Comparison, internal->header.type);
    EXPECT_EQ(13u, Platform::bigEndianUInteger32ValueAt(&internal->header.size));
    EXPECT_EQ(Common::Property::TypeID::Color, internal->tagID);
    EXPECT_EQ(TrackPredicate::ComparisonOperator::EndsWith, internal->comparisonOperator);
    EXPECT_STREQ("hello", internal->value);
}

TEST_F(TrackPredicateTests, ACompoundPredicate_compoundWith_ReturnsCorrectPredicate)
{
    // -- Given.
    auto predicate1 = TrackPredicate::comparisonWith(Common::Property::TypeID::Color, TrackPredicate::ComparisonOperator::EndsWith, "hello");
    auto predicate2 = TrackPredicate::comparisonWith(Common::Property::TypeID::BeatsPerMinute, TrackPredicate::ComparisonOperator::LessThan, "10.5");
    auto predicate = TrackPredicate::compoundWith(predicate1, TrackPredicate::CompoundOperator::And, predicate2);

    // -- When.
    auto result = predicate.asBlob();

    // -- Then.
    EXPECT_EQ(31u, result.size());
    auto internal = reinterpret_cast<const TrackPredicate::CompoundDataHeader*>(result.data().get());
    EXPECT_EQ(TrackPredicate::Type::Compound, internal->header.type);
    EXPECT_EQ(31u, Platform::bigEndianUInteger32ValueAt(&internal->header.size));
    EXPECT_EQ(TrackPredicate::CompoundOperator::And, internal->compoundOperator);

    auto leftSide = reinterpret_cast<const TrackPredicate::ComparisonDataHeader*>(&internal->leftPredicate);
    EXPECT_EQ(TrackPredicate::Type::Comparison, leftSide->header.type);
    EXPECT_EQ(13u, Platform::bigEndianUInteger32ValueAt(&leftSide->header.size));
    EXPECT_EQ(Common::Property::TypeID::Color, leftSide->tagID);
    EXPECT_EQ(TrackPredicate::ComparisonOperator::EndsWith, leftSide->comparisonOperator);
    EXPECT_STREQ("hello", leftSide->value);
    auto rightSide = reinterpret_cast<const TrackPredicate::ComparisonDataHeader*>(internal->leftPredicate + Platform::bigEndianUInteger32ValueAt(&leftSide->header.size));
    EXPECT_EQ(TrackPredicate::Type::Comparison, rightSide->header.type);
    EXPECT_EQ(12u, Platform::bigEndianUInteger32ValueAt(&rightSide->header.size));
    EXPECT_EQ(Common::Property::TypeID::BeatsPerMinute, rightSide->tagID);
    EXPECT_EQ(TrackPredicate::ComparisonOperator::LessThan, rightSide->comparisonOperator);
    EXPECT_STREQ("10.5", rightSide->value);
}

} }
