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
#include <Base/Duration.hpp>

using namespace testing;

namespace NxA {

class DurationTests : public NxA::Test
{

};

TEST_F(DurationTests, Duration_Addition_CorrectValueIsComputed)
{
    // -- Given.
    auto value1 = 10_Hours + 12_Minutes;
    auto value2 = 12_Hours + 10_Minutes;

    // -- When.
    auto value = value1 + value2;

    // -- Then.
    EXPECT_TRUE(value > value1);
    EXPECT_TRUE(value > value2);
    EXPECT_EQ(value.toSeconds<integer64>(), 80520);
}

TEST_F(DurationTests, Duration_AdditionMilliseconds_CorrectValueIsComputed)
{
    // -- Given.
    auto value1 = 10_Hours + 12_Minutes;
    auto value2 = 5000_Milliseconds;

    // -- When.
    auto value = value1 + value2;

    // -- Then.
    EXPECT_TRUE(value > value1);
    EXPECT_TRUE(value > value2);
    EXPECT_EQ(value.toSeconds<integer64>(), 36725);
}

TEST_F(DurationTests, Duration_DurationHz_CorrectNumberOfSteps)
{
    // -- Given.
    auto value = Duration::fromBeatAtBPM(120ll);

    // -- When.
    EXPECT_TRUE(value.isValid());
    auto half = *value / 2;
    EXPECT_TRUE(half.isValid());
    auto tripled = *half * 3;
    auto tbpm = tripled.toBPM<integer64>();
    auto hbpm = half->toBPM<integer64>();

    // -- Then.
    EXPECT_TRUE(*half < *value);
    EXPECT_TRUE(tripled > *value);
    EXPECT_EQ(*value, Duration::fromHz<2>(1));
    EXPECT_EQ(*half, Duration::fromHz<4>(1));
    EXPECT_EQ(tripled, Duration::fromHz<4>(3));
    EXPECT_TRUE(tbpm.isValid());
    EXPECT_EQ(*tbpm, 80);
    EXPECT_TRUE(hbpm.isValid());
    EXPECT_EQ(*hbpm, 240);
}

TEST_F(DurationTests, Duration_DurationDivision_CorrectValueIsComputed)
{
    // -- Given.
    auto value = 10_Hours + 12_Minutes;

    // -- When.
    auto half = value / 2ll;

    // -- Then.
    EXPECT_TRUE(half.isValid());
    EXPECT_TRUE(*half < value);
    EXPECT_TRUE(value > *half);
    EXPECT_EQ(*half * 2, value);
    EXPECT_EQ(*half + *half, value);
    EXPECT_EQ(half->toSeconds<integer64>(), 18360);
    EXPECT_EQ(value.toSeconds<integer64>(), 36720);
}

TEST_F(DurationTests, Duration_DurationFromFloats_ConvertedCorrectly)
{
    // -- Given.
    constexpr auto value1 = 10.14_Hours + 12.2_Minutes + 15.3_Seconds;

    // -- When.
    auto hms1 = value1.toHoursMinutesSeconds();

    // -- Then.
    EXPECT_EQ(hms1.hours, 10);
    EXPECT_EQ(hms1.minutes, 20);
    EXPECT_EQ(hms1.seconds, 51);
    EXPECT_EQ(hms1.milliseconds, 300);
}

TEST_F(DurationTests, Duration_DurationFromHours_AllRepresentable)
{
    for (integer64 hour = 0; hour < 1000; ++hour) {
        // -- Given.
        auto duration = Duration::fromHours(hour);

        // -- When.
        duration = duration + 1_Seconds;
        duration = duration - 1_Seconds;

        // -- Then.
        EXPECT_EQ(duration.toHours(), hour);
    }
}

TEST_F(DurationTests, Duration_DurationFromMinutes_AllRepresentable)
{
    for (integer64 minute = 0; minute < 60; ++minute) {
        // -- Given.
        auto duration = Duration::fromMinutes(minute);

        // -- When.
        duration = duration + 1_Seconds;
        duration = duration - 1_Seconds;

        // -- Then.
        EXPECT_EQ(duration.toMinutes(), minute);
    }
}

TEST_F(DurationTests, Duration_DurationFromSeconds_AllRepresentable)
{
    for (integer64 sec = 0; sec < 600; ++sec) {
        // -- Given.
        auto duration = Duration::fromSeconds(sec);

        // -- When.
        duration = duration + 1_Seconds;
        duration = duration - 1_Seconds;

        // -- Then.
        EXPECT_EQ(duration.toSeconds(), sec);
    }
}

TEST_F(DurationTests, Duration_DurationFromMilliseconds_AllRepresentable)
{
    for (integer64 milli = 0; milli < 1000; ++milli) {
        // -- Given.
        auto duration = Duration::fromMilliseconds(milli);

        // -- When.
        duration = duration + 1_Seconds;
        duration = duration - 1_Seconds;

        // -- Then.
        EXPECT_EQ(duration.toMilliseconds(), milli);
    }
}

TEST_F(DurationTests, Duration_DurationFromMicroseconds_AllRepresentable)
{
    for (integer64 micro = 0; micro < 1000; ++micro) {
        // -- Given.
        auto duration = Duration::fromMicroseconds(micro);

        // -- When.
        duration = duration + 1_Seconds;
        duration = duration - 1_Seconds;

        // -- Then.
        EXPECT_EQ(duration.toMicroseconds(), micro);
    }
}

TEST_F(DurationTests, Duration_SingleFlick_FivePenFlicks)
{
    // -- Given.
    constexpr auto oneFlick = Duration::fromFlicks(1);

    // -- When.
    constexpr auto fivePenFlicks = Duration::fromPenFlicks(5);

    // -- Then.
    EXPECT_EQ(oneFlick, fivePenFlicks);
}

TEST_F(DurationTests, Duration_DurationCombinedWithSamples_CorrectSongDuration)
{
    // -- Given.
    constexpr auto songLength = 12_Minutes + 15_Seconds + 102_Milliseconds;

    // -- When.
    auto songLengthFromSamples = Duration::fromHz<44100>(1) *  32417998 + Duration::fromFlicks(3200);

    // -- Then.
    EXPECT_EQ(Duration::fromHz<44100>(1) *  32417998, Duration::fromHz<44100>(32417998));
    EXPECT_EQ(songLength, songLengthFromSamples);
}

TEST_F(DurationTests, Duration_DurationDividedWithRemainder_CorrectSampleCount)
{
    // -- Given.
    constexpr auto songLength = 12_Minutes + 15_Seconds + 102_Milliseconds;

    // -- When.
    auto maybeSamples = songLength.divideByDurationWithRemainder(Duration::fromHz<44100>(1));
    EXPECT_TRUE(maybeSamples.isValid());
    auto& [samples, leftover] = *maybeSamples;

    // -- Then.
    EXPECT_EQ(samples, 32417998);
    EXPECT_EQ(leftover.toFlicks(), 3200);
}

TEST_F(DurationTests, Duration_DurationToHMS_CorrectValueIsComputed)
{
    // -- Given.
    constexpr auto value1 = 10_Hours + 12_Minutes + 15_Seconds;
    auto value2 = value1 + 10_Hours + 92_Minutes + 72_Seconds + 128376_Milliseconds + 123_Microseconds;

    // -- When.
    auto hms1 = value1.toHoursMinutesSeconds();
    auto hms2 = value2.toHoursMinutesSeconds();

    // -- Then.
    EXPECT_EQ(hms1.hours, 10);
    EXPECT_EQ(hms1.minutes, 12);
    EXPECT_EQ(hms1.seconds, 15);
    EXPECT_EQ(hms1.milliseconds, 0);

    EXPECT_EQ(hms2.hours, 21);
    EXPECT_EQ(hms2.minutes, 47);
    EXPECT_EQ(hms2.seconds, 35);
    EXPECT_EQ(hms2.milliseconds, 376);
}

TEST_F(DurationTests, Duration_DurationFlicks_CorrectFlickCounts)
{
    // -- Given.
    auto film = Duration::fromCycleAtHz(24);
    auto tv = Duration::fromCycleAtHz(60);
    auto cdaudio = Duration::fromCycleAtHz(44100);
    auto hqaudio = Duration::fromCycleAtHz(96000);
    auto bpm = Duration::fromBeatAtBPM(110.08);
    auto hps = Duration::fromFlicks(392000000L);

    // -- When.
    EXPECT_TRUE(film.isValid());
    EXPECT_TRUE(tv.isValid());
    EXPECT_TRUE(cdaudio.isValid());
    EXPECT_TRUE(hqaudio.isValid());
    EXPECT_TRUE(bpm.isValid());
    auto filmFlicks = film->toFlicks();
    auto tvFlicks = tv->toFlicks();
    auto cdaudioFlicks = cdaudio->toFlicks();
    auto hqaudioFlicks = hqaudio->toFlicks();
    auto bpmFlicks = bpm->toFlicks();

    // -- Then.
    EXPECT_EQ(hps.toFlicks(), 392000000L);
    EXPECT_EQ(filmFlicks, 29400000L);
    EXPECT_EQ(tvFlicks, 11760000L);
    EXPECT_EQ(cdaudioFlicks, 16000L);
    EXPECT_EQ(hqaudioFlicks, 7350L);
    EXPECT_EQ(bpmFlicks, 384593023L);
    EXPECT_EQ(*hps.toBPM<double>(), 108.0);
}

}
