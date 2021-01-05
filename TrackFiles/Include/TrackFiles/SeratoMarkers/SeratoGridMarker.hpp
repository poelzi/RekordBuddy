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

#include <TrackFiles/SeratoMarkers/SeratoMarker.hpp>

#include <Base/Base.hpp>

namespace NxA {

// -- Public Interface
class SeratoGridMarker
{
    // -- Types
    struct GridMarkerStruct {
        byte positionInSeconds[4];
        byte beatsPerMinute[4];
    };

    // -- Instance Variables
    DecimalNumber p_positionInSeconds;
    DecimalNumber p_beatsPerMinute;

    // -- Class Methods
    static count p_actualNumberOfBeatsIfSupported(const DecimalNumber& numberOfBeats)
    {
        auto asIntegerWith3DigitOfPrecision = (numberOfBeats * 1000).asInteger(DecimalNumber::UsingRoundingPolicy::AwayFromZero);
        auto decimalPart = asIntegerWith3DigitOfPrecision % 1000;

        if (decimalPart >= 950) {
            asIntegerWith3DigitOfPrecision += 51;
        }
        else if (decimalPart > 50) {
            return 0;
        }

        auto integerPart = asIntegerWith3DigitOfPrecision / 1000;

        // -- Serato grid marker need to be on a first downbeat.
        if (integerPart % 4) {
            return 0;
        }
        else {
            return integerPart;
        }
    }

public:
    // -- Factory Methods
    static SeratoGridMarker markerWithPositionAndBeatsPerMinute(const DecimalNumber&, const DecimalNumber&);

    // -- Class Methods
    static void addMarkersWithMemoryAtTo(const byte*, count, MutableArray<SeratoMarker::OfSomeSort>&);
    static Blob gridMarkerDataFrom(const MutableArray<SeratoMarker::OfSomeSort>&);
    static boolean gridMarkersAreValid(const Array<SeratoGridMarker>&);
    static bool isValidV1RawMarker(const byte*);
    static bool isValidV1EncodedMarker(const byte*);
    static integer32 sizeOfV1RawMarker();
    static integer32 sizeOfV1EncodedMarker();

    // -- Constructors & Destructors
    SeratoGridMarker() : p_positionInSeconds{ "0" }, p_beatsPerMinute{ "0" } { }
    ~SeratoGridMarker() = default;

    // -- Operators
    bool operator==(const SeratoGridMarker& other) const noexcept
    {
        return (this->p_positionInSeconds == other.p_positionInSeconds) &&
               (this->p_beatsPerMinute == other.p_beatsPerMinute);
    }
    bool operator!=(const SeratoGridMarker& other) const noexcept
    {
        return !this->operator==(other);
    }
    bool operator<(const SeratoGridMarker& other) const noexcept
    {
        return (this->p_positionInSeconds < other.p_positionInSeconds) ||
               (this->p_beatsPerMinute < other.p_beatsPerMinute);
    }

    // -- Instance Methods
    const DecimalNumber& positionInSeconds() const
    {
        return this->p_positionInSeconds;
    }
    String positionInSecondsAsString() const
    {
        return this->p_positionInSeconds.asStringWithFractionDigitsBetween(3, 3, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    }
    void setPositionInSeconds(const DecimalNumber& position)
    {
        this->p_positionInSeconds = position;
    }

    const DecimalNumber& beatsPerMinute() const
    {
        return this->p_beatsPerMinute;
    }
    String beatsPerMinuteAsString() const
    {
        return this->p_beatsPerMinute.asStringWithFractionDigitsBetween(2, 2, DecimalNumber::UsingRoundingPolicy::AwayFromZero);
    }
    void setBeatsPerMinute(const DecimalNumber& beatsPerMinute)
    {
        this->p_beatsPerMinute = beatsPerMinute;
    }

    void addMarkerV2TagTo(MutableBlob&) const;
    void addRawMarkerV1TagTo(MutableBlob&) const;
    void addEncodedMarkerV1TagTo(MutableBlob&) const;
};

}
