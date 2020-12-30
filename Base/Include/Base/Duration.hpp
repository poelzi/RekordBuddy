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

#pragma once

#include <Base/Types.hpp>
#include <Base/Optional.hpp>

#include <chrono>
#include <ratio>

namespace NxA {

struct HoursMinutesSeconds
{
    long long hours;
    long long minutes;
    long long seconds;
    long long milliseconds;
    long long microseconds;
};

class Duration;

class StartOfMeasurement final
{
    friend Duration;
    std::chrono::high_resolution_clock::time_point triggerTime;
    explicit StartOfMeasurement(std::chrono::high_resolution_clock::time_point withTriggerTime) :triggerTime{withTriggerTime} {}
public:
    Duration duration() const;
};

class Duration final
{
    // -- Duration is represented as penflicks, which are exactly 5 flicks.
    // -- Flicks are a small unit of time that are very evenly divisible by common file format time durations; i.e., for common durations they will have no rounding.
    // -- Even if there is rounding, it is a very short amount of time -- 0.283 nanoseconds, so rounding is typically not a concern.
    // -- Due to Nyquist limit, this type should not be used for frequencies higher than ~1.7ghz
    // -- Assuming long long is 64bit, Duration (penflicks) can represent any duration as an exact count of penflicks up to ~82.8 years in length, positive or negative.
    // -- For original idea, see here: https://www.facebook.com/christopher.horvath.395/posts/1157292757692660
    // -- 24 fps frame:     29400000 flicks
    // -- 25 fps frame:     28224000 flicks
    // -- 30 fps frame:     23520000 flicks
    // -- 48 fps frame:     14700000 flicks
    // -- 50 fps frame:     14112000 flicks
    // -- 60 fps frame:     11760000 flicks
    // -- 90 fps frame:      7840000 flicks
    // -- 100 fps frame:     7056000 flicks
    // -- 120 fps frame:     5880000 flicks
    // -- We can also do common audio rates with precise numbers of flicks
    // -- 8000 hz:      88200 flicks
    // -- 16000 hz:     44100 flicks
    // -- 22050 hz:     32000 flicks
    // -- 24000 hz:     29400 flicks
    // -- 32000 hz:     22050 flicks
    // -- 44100 hz:     16000 flicks
    // -- 48000 hz:     14700 flicks
    // -- 88200 hz:      8000 flicks
    // -- 96000 hz:      7350 flicks
    // -- 192000 hz:     3675 flicks, or 18375 penflicks.
    // -- In addition, each microsecond is 3528 penflicks.
    // -- A penflick is 283.4 picoseconds, 0.2834 nanoseconds.
    using PenFlickRatio = std::ratio<1, 3528000000>;
    using FlickRatio = std::ratio<1, 705600000>;
    using Flick = std::chrono::duration<long long, FlickRatio>;
    using PenFlick = std::chrono::duration<long long, PenFlickRatio>;

    template<typename T, typename R = std::ratio<1ll,1ll>>
        using GenericDuration = std::chrono::duration<typename std::common_type<T, long long>::type, R>;

    PenFlick p_duration;

public:
    // -- Constructors & Destructors
    Duration() = default;
    constexpr Duration(const Duration& value) = default;
    template <typename T, typename Unit>
        explicit constexpr Duration(const std::chrono::duration<T, Unit>& value) : p_duration{std::chrono::duration_cast<PenFlick>(value)}
        {
        }

    template <typename T>
        explicit constexpr Duration(const T& value) : Duration{std::chrono::duration<T>(value)}
        {
        }

    // -- Factory Methods
    //! each thread can start measuring a duration, and then use durationSinceStarting to gather the Duration since that start moment.
    static StartOfMeasurement startMeasuringDuration()
    {
        return StartOfMeasurement{std::chrono::high_resolution_clock::now()};
    }

    //! each thread can start measuring a duration with startMeasuringDuration, and then gather the Duration since that start moment.
    static Duration durationSinceStarting(StartOfMeasurement time)
    {
        return Duration{std::chrono::high_resolution_clock::now() - time.triggerTime};
    }

    static constexpr Duration fromPenFlicks(long long value)
    {
        return Duration{GenericDuration<long long, PenFlickRatio>(value)};
    }

    static constexpr Duration fromFlicks(long long value)
    {
        return Duration{GenericDuration<long long, FlickRatio>(value)};
    }

    template <typename T>
        static constexpr Duration fromMilliseconds(T value)
        {
            return Duration{GenericDuration<T, std::milli>(value)};
        }

    template <typename T>
        static constexpr Duration fromMicroseconds(T value)
        {
            return Duration{GenericDuration<T, std::micro>(value)};
        }

    template <typename T>
        static constexpr Duration fromSeconds(T value)
        {
            return Duration{GenericDuration<T>(value)};
        }

    template <typename T>
        static constexpr Duration fromMinutes(T value)
        {
            return Duration{GenericDuration<T, std::ratio<60>>(value)};
        }

    template <typename T>
        static constexpr Duration fromHours(T value)
        {
            return Duration{GenericDuration<T, std::ratio<3600>>(value)};
        }

    //! given some hz, gives us the duration of one cycle at that hz. Only defined for positive values.
    template <typename T>
        static constexpr Optional<Duration> fromCycleAtHz(T hz)
        {
            if (hz <= 0) {
                return nothing;
            }
            return Duration{std::chrono::duration_cast<PenFlick>(std::chrono::seconds(1)) / hz};
        }

    //! given some bpm, gives us the duration of one beat at that bpm. Only defined for positive bpm values.
    template <typename T>
        static constexpr Optional<Duration> fromBeatAtBPM(T bpm)
        {
            if (bpm <= 0) {
                return nothing;
            }
            return Duration{std::chrono::duration_cast<PenFlick>(std::chrono::minutes(1)) / bpm};
        }

    //! value is a number of beats. e.g., to get the duration of 5 beats at 60bpm: fromBPM<60>(5)
    template <long long bpm, typename T>
        static constexpr Duration fromBPM(T value)
        {
            static_assert(bpm > 0, "BPM must be a positive value");
            return Duration{GenericDuration<T, std::ratio<60, bpm>>(value)};
        }

    //! value is in cycles. e.g., to get the duration of 500 cycles at 440hz: fromHz<440>(500)
    template <long long frequency, typename T>
        static constexpr Duration fromHz(T value)
        {
            static_assert(frequency > 0, "Hz must be a positive value");
            return Duration{GenericDuration<T, std::ratio<1, frequency>>(value)};
        }

    // -- Instance Methods
    template<class Rep, class Period>
        constexpr const std::chrono::duration<Rep, Period> toStdDuration() const
        {
            return std::chrono::duration_cast<GenericDuration<Rep, Period>>(p_duration);
        }

    constexpr long long toPenFlicks() const
    {
        return this->toStdDuration<long long, PenFlickRatio>().count();
    }

    constexpr long long toFlicks() const
    {
        return this->toStdDuration<long long, FlickRatio>().count();
    }

    template <typename T=long long>
        constexpr T toDays() const
        {
            return this->toHours() / 24;
        }

    template <typename T=long long>
        constexpr T toHours() const
        {
            return std::chrono::duration_cast<GenericDuration<T, std::ratio<3600>>>(p_duration).count();
        }

    template <typename T=long long>
        constexpr T toMinutes() const
        {
            return std::chrono::duration_cast<GenericDuration<T, std::ratio<60>>>(p_duration).count();
        }

    template <typename T=long long>
        constexpr T toSeconds() const
        {
            return std::chrono::duration_cast<GenericDuration<T>>(p_duration).count();
        }

    template <typename T=long long>
        constexpr T toMicroseconds() const
        {
            return std::chrono::duration_cast<GenericDuration<T, std::micro>>(p_duration).count();
        }

    template <typename T=long long>
        constexpr T toMilliseconds() const
        {
            return std::chrono::duration_cast<GenericDuration<T, std::milli>>(p_duration).count();
        }

    //! If the duration is the length of one complete cycle, how many cycles can we get into a second?
    template <typename T=long long>
        constexpr Optional<T> toHz() const
        {
            return Duration::fromSeconds(1ll).divideByDuration<T>(*this);
        }

    //! If the duration is the length of one beat, how many of them can we get into a minute?
    template <typename T=long long>
        constexpr Optional<T> toBPM() const
        {
            return Duration::fromMinutes(1ll).divideByDuration<T>(*this);
        }

    //! How many of given duration fit into this duration (expressed in T). For example, exactly how many samples will a given recording take: auto samples = songLength.divideByDuration(Duration::fromHz<44100>(1))
    template<typename T=long long>
        constexpr Optional<T> divideByDuration(const Duration& b) const
        {
            if (b == Duration{}) {
                return nothing;
            }
            return std::chrono::duration_cast<GenericDuration<T, PenFlickRatio>>(p_duration) / b.p_duration;
        }

    //! Divide a duration into a fixed number of parts
    template<long long numberOfParts>
        constexpr Duration divideIntoParts() const
        {
            static_assert(numberOfParts > 0, "numberOfParts must be a positive value");
            return Duration{ p_duration / numberOfParts};
        }

    //! How many of given duration fit into this duration, with remainder (duration after accounting for the division).
    constexpr Optional<std::tuple<long long, Duration>> divideByDurationWithRemainder(const Duration& b) const
    {
        if (b == Duration{}) {
            return nothing;
        }
        return {std::tuple<long long, Duration>{ p_duration / b.p_duration, Duration{ p_duration % b.p_duration }}};
    }


    constexpr HoursMinutesSeconds toHoursMinutesSeconds() const
    {
        auto hours = std::chrono::duration_cast<std::chrono::hours>(p_duration).count();
        auto minutes = std::chrono::duration_cast<std::chrono::minutes>(p_duration % std::chrono::hours(1)).count();
        auto seconds = std::chrono::duration_cast<std::chrono::seconds>(p_duration % std::chrono::minutes(1)).count();
        auto milliseconds = std::chrono::duration_cast<std::chrono::milliseconds>(p_duration % std::chrono::seconds(1)).count();
        auto microseconds = std::chrono::duration_cast<std::chrono::microseconds>(p_duration % std::chrono::milliseconds(1)).count();
        return { hours, minutes, seconds, milliseconds, microseconds };
    }

    // -- Comparison operators

    constexpr bool operator==(const Duration& b) const
    {
        return p_duration == b.p_duration;
    }
    constexpr bool operator!=(const Duration& b) const
    {
        return p_duration != b.p_duration;
    }
    constexpr bool operator>=(const Duration& b) const
    {
        return p_duration >= b.p_duration;
    }
    constexpr bool operator<=(const Duration& b) const
    {
        return p_duration <= b.p_duration;
    }
    constexpr bool operator>(const Duration& b) const
    {
        return p_duration > b.p_duration;
    }
    constexpr bool operator<(const Duration& b) const
    {
        return p_duration < b.p_duration;
    }

    // -- Algebraic operators

    constexpr Duration operator%(const Duration& b) const
    {
        return Duration{ p_duration % b.p_duration };
    }
    constexpr Duration operator-(const Duration& b) const
    {
        return Duration{ p_duration - b.p_duration };
    }
    constexpr Duration operator+(const Duration& b) const
    {
        return Duration{ p_duration + b.p_duration };
    }
    template <typename T>
        constexpr Duration operator*(const T& b) const
        {
            return Duration{ p_duration * b};
        }
    template <typename T>
        constexpr Optional<Duration> operator/(const T& b) const
        {
            if (b == 0) {
                return nothing;
            }
            return Duration{GenericDuration<T, PenFlickRatio>(p_duration) / b};
        }
};

inline Duration StartOfMeasurement::duration() const
{
    return Duration::durationSinceStarting(*this);
}

// -- User-defined literals
constexpr Duration operator"" _Hours(long double n)
{
    return Duration::fromHours(n);
}

constexpr Duration operator"" _Minutes(long double n)
{
    return Duration::fromMinutes(n);
}

constexpr Duration operator"" _Seconds(long double n)
{
    return Duration::fromSeconds(n);
}

constexpr Duration operator"" _Milliseconds(long double n)
{
    return Duration::fromMilliseconds(n);
}

constexpr Duration operator"" _Microseconds(long double n)
{
    return Duration::fromMicroseconds(n);
}

constexpr Duration operator"" _Hours(unsigned long long n)
{
    return Duration{std::chrono::hours(n)};
}

constexpr Duration operator"" _Minutes(unsigned long long n)
{
    return Duration{std::chrono::minutes(n)};
}

constexpr Duration operator"" _Seconds(unsigned long long n)
{
    return Duration{std::chrono::seconds(n)};
}

constexpr Duration operator"" _Milliseconds(unsigned long long n)
{
    return Duration{std::chrono::milliseconds(n)};
}

constexpr Duration operator"" _Microseconds(unsigned long long n)
{
    return Duration{std::chrono::microseconds(n)};
}

}
