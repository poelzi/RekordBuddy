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

#include <CommonCollection/Tracks/TrackRating.hpp>

#include <Base/Test.hpp>

using namespace testing;

namespace NxA { namespace Common {

class TrackRatingTests : public NxA::Test
{

};

TEST_F(TrackRatingTests, maybeWithValue_DifferentRawRatings_ReturnsTheCorrectObject)
{
    // -- Given.
    // -- When.
    // -- Then.
    auto rating = TrackRating::maybeWithValue(-23);
    EXPECT_FALSE(rating.isValid());
    rating = TrackRating::maybeWithValue(-1);
    EXPECT_FALSE(rating.isValid());
    rating = TrackRating::maybeWithValue(0);
    ASSERT_TRUE(rating.isValid());
    EXPECT_EQ(0u, rating->value());
    rating = TrackRating::maybeWithValue(200);
    ASSERT_TRUE(rating.isValid());
    EXPECT_EQ(200u, rating->value());
    rating = TrackRating::maybeWithValue(255);
    ASSERT_TRUE(rating.isValid());
    EXPECT_EQ(255u, rating->value());
    rating = TrackRating::maybeWithValue(256);
    EXPECT_FALSE(rating.isValid());
    rating = TrackRating::maybeWithValue(500);
    EXPECT_FALSE(rating.isValid());
}

TEST_F(TrackRatingTests, ConstructorFromStarCount_DifferentStarValues_ReturnsTheCorrectValue)
{
    // -- Given.
    // -- When.
    // -- Then.
    EXPECT_EQ(0u, TrackRating{ TrackRating::Stars::Zero }.value());
    EXPECT_EQ(51u, TrackRating{ TrackRating::Stars::One }.value());
    EXPECT_EQ(102u, TrackRating{ TrackRating::Stars::Two }.value());
    EXPECT_EQ(153u, TrackRating{ TrackRating::Stars::Three }.value());
    EXPECT_EQ(204u, TrackRating{ TrackRating::Stars::Four }.value());
    EXPECT_EQ(255u, TrackRating{ TrackRating::Stars::Five }.value());
}

TEST_F(TrackRatingTests, ConstructorFromStarCount_DifferentStarValues_ReturnsTheSameInitialValue)
{
    // -- Given.
    // -- When.
    // -- Then.
    EXPECT_EQ(TrackRating::Stars::Zero, TrackRating{ TrackRating::Stars::Zero }.asStarCount());
    EXPECT_EQ(TrackRating::Stars::One, TrackRating{ TrackRating::Stars::One }.asStarCount());
    EXPECT_EQ(TrackRating::Stars::Two, TrackRating{ TrackRating::Stars::Two }.asStarCount());
    EXPECT_EQ(TrackRating::Stars::Three, TrackRating{ TrackRating::Stars::Three }.asStarCount());
    EXPECT_EQ(TrackRating::Stars::Four, TrackRating{ TrackRating::Stars::Four }.asStarCount());
    EXPECT_EQ(TrackRating::Stars::Five, TrackRating{ TrackRating::Stars::Five }.asStarCount());
}

TEST_F(TrackRatingTests, asStarCount_DifferentRawRatings_ReturnsTheCorrectValue)
{
    // -- Given.
    // -- When.
    // -- Then.
    EXPECT_EQ(TrackRating::Stars::Zero, TrackRating::maybeWithValue(0)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::One, TrackRating::maybeWithValue(1)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::One, TrackRating::maybeWithValue(35)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::One, TrackRating::maybeWithValue(51)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Two, TrackRating::maybeWithValue(52)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Two, TrackRating::maybeWithValue(89)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Two, TrackRating::maybeWithValue(102)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Three, TrackRating::maybeWithValue(103)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Three, TrackRating::maybeWithValue(140)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Three, TrackRating::maybeWithValue(153)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Four, TrackRating::maybeWithValue(154)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Four, TrackRating::maybeWithValue(200)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Four, TrackRating::maybeWithValue(204)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Five, TrackRating::maybeWithValue(205)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Five, TrackRating::maybeWithValue(223)->asStarCount());
    EXPECT_EQ(TrackRating::Stars::Five, TrackRating::maybeWithValue(255)->asStarCount());
}

TEST_F(TrackRatingTests, asString_DifferentRawRatings_ReturnsTheCorrectString)
{
    // -- Given.
    // -- When.
    // -- Then.
    EXPECT_EQ(""_String, TrackRating::maybeWithValue(0)->asString());
    EXPECT_EQ("⭐"_String, TrackRating::maybeWithValue(1)->asString());
    EXPECT_EQ("⭐"_String, TrackRating::maybeWithValue(35)->asString());
    EXPECT_EQ("⭐"_String, TrackRating::maybeWithValue(51)->asString());
    EXPECT_EQ("⭐⭐"_String, TrackRating::maybeWithValue(52)->asString());
    EXPECT_EQ("⭐⭐"_String, TrackRating::maybeWithValue(89)->asString());
    EXPECT_EQ("⭐⭐"_String, TrackRating::maybeWithValue(102)->asString());
    EXPECT_EQ("⭐⭐⭐"_String, TrackRating::maybeWithValue(103)->asString());
    EXPECT_EQ("⭐⭐⭐"_String, TrackRating::maybeWithValue(140)->asString());
    EXPECT_EQ("⭐⭐⭐"_String, TrackRating::maybeWithValue(153)->asString());
    EXPECT_EQ("⭐⭐⭐⭐"_String, TrackRating::maybeWithValue(154)->asString());
    EXPECT_EQ("⭐⭐⭐⭐"_String, TrackRating::maybeWithValue(200)->asString());
    EXPECT_EQ("⭐⭐⭐⭐"_String, TrackRating::maybeWithValue(204)->asString());
    EXPECT_EQ("⭐⭐⭐⭐⭐"_String, TrackRating::maybeWithValue(205)->asString());
    EXPECT_EQ("⭐⭐⭐⭐⭐"_String, TrackRating::maybeWithValue(223)->asString());
    EXPECT_EQ("⭐⭐⭐⭐⭐"_String, TrackRating::maybeWithValue(255)->asString());
}

} }
