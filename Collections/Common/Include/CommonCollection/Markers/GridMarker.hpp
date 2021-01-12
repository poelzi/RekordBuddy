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

#include <CommonCollection/Markers/Marker.hpp>

#include <Base/Array.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/String.hpp>
#include <Base/Flags.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class GridMarker
{
public:
    enum class BeatNumber : byte {
        // -- These are stored in user data and cannot be re-ordered or modified.
        // -- They all need to be unique.
        FirstDownBeat,
        SecondDownBeat,
        ThirdDownBeat,
        FourthDownBeat,
    };

    // -- Class Methods
    static inline BeatNumber beatNumberAfter(BeatNumber beatNumber)
    {
        switch (beatNumber) {
            case BeatNumber::FirstDownBeat: {
                return BeatNumber::SecondDownBeat;
            }
            case BeatNumber::SecondDownBeat: {
                return BeatNumber::ThirdDownBeat;
            }
            case BeatNumber::ThirdDownBeat: {
                return BeatNumber::FourthDownBeat;
            }
            case BeatNumber::FourthDownBeat: {
                return BeatNumber::FirstDownBeat;
            }
        }
    }
    static inline Optional<BeatNumber> maybeBeatNumberFromInteger(integer beatNumberAsInteger)
    {
        switch (beatNumberAsInteger) {
            case 1: {
                return BeatNumber::FirstDownBeat;
            }
            case 2: {
                return BeatNumber::SecondDownBeat;
            }
            case 3: {
                return BeatNumber::ThirdDownBeat;
            }
            case 4: {
                return BeatNumber::FourthDownBeat;
            }
        }

        return nothing;
    }

    // -- Constructors & Destructors
    virtual ~GridMarker() = default;

    // -- Operators
    bool operator==(const GridMarker& other) const
    {
        return (this->positionInSeconds() == other.positionInSeconds()) &&
               (this->flags() == other.flags()) &&
               (this->beatNumber() == other.beatNumber()) &&
               (this->beatsPerMinute() == other.beatsPerMinute());
    }
    inline bool operator!=(const GridMarker& other) const
    {
        return !this->operator==(other);
    }

    // -- Instance Methods
    virtual DecimalNumber positionInSeconds() const = 0;

    virtual Common::Marker::Flags flags() const = 0;

    // -- This is not used or supported internally by Rekord Buddy but can be used on output
    // -- after validation for certain collection types.
    virtual Optional<count> maybeHotCueNumber() const
    {
        return nothing;
    }

    virtual BeatNumber beatNumber() const = 0;

    virtual DecimalNumber beatsPerMinute() const = 0;

    String description() const
    {
        auto result = MutableString::stringWithFormat("GRID at %s seconds, %s bpm",
                                                      this->positionInSeconds().asString().asUTF8(),
                                                      this->beatsPerMinute().asString().asUTF8());

        auto maybeHotCue = this->maybeHotCueNumber();
        if (maybeHotCue.isValid()) {
            result.appendStringWithFormat(", hotcue #%llu", *maybeHotCue);
        }

        switch (this->beatNumber()) {
            case BeatNumber::FirstDownBeat: {
                result.append(", first downbeat"_String);
                break;
            }
            case BeatNumber::SecondDownBeat: {
                result.append(", second downbeat"_String);
                break;
            }
            case BeatNumber::ThirdDownBeat: {
                result.append(", third downbeat"_String);
                break;
            }
            case BeatNumber::FourthDownBeat: {
                result.append(", fourth downbeat"_String);
                break;
            }
        }

        return result;
    }
};

class MutableGridMarker : public GridMarker
{
protected:
    virtual void p_setBeatsPerMinute(const DecimalNumber&) = 0;

public:
    // -- Constructors & Destructors
    virtual ~MutableGridMarker() = default;

    // -- Instance Methods
    virtual void setPositionInSeconds(const DecimalNumber&) = 0;

    virtual void setFlag(Common::Marker::Flag) = 0;
    virtual void setFlags(const Common::Marker::Flags&) = 0;
    virtual void clearFlag(Common::Marker::Flag) = 0;

    // -- This is not used or supported internally by Rekord Buddy but can be used on output
    // -- after validation for certain collection types.
    virtual void setHotCueNumber(const Optional<count>& maybeHotCueNumber)
    {
        NXA_ALOG("This is not supported internally.");
    }

    virtual void setBeatNumber(BeatNumber) = 0;

    virtual void setBeatsPerMinute(const DecimalNumber& beatsPerMinute)
    {
        if (beatsPerMinute.isZero()) {
            // -- We don't want to set invalid BPM values.
            return;
        }

        this->p_setBeatsPerMinute(beatsPerMinute);
    }

    template <class T>
        void setWithSamePropertiesAsAndMaybeAddOffsetInSeconds(const T& other, Optional<DecimalNumber> maybeOffset = nothing)
        {
            if (maybeOffset.isValid()) {
                this->setPositionInSeconds(other.positionInSeconds() + *maybeOffset);
            }
            else {
                this->setPositionInSeconds(other.positionInSeconds());
            }
            this->setFlags(other.flags());
            this->setBeatNumber(other.beatNumber());
            this->setBeatsPerMinute(other.beatsPerMinute());
        }
};

} }
