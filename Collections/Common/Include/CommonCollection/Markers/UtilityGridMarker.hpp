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

#include <CommonCollection/Markers/GridMarker.hpp>

#include <Base/DecimalNumber.hpp>
#include <Base/Flags.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class MutableUtilityGridMarker : public Common::MutableGridMarker
{
    // -- Private Instance Variables
    DecimalNumber p_positionInSeconds;

    Common::Marker::Flags p_flags;

    Optional<count> p_maybeHotCueNumber;

    Common::GridMarker::BeatNumber p_beatNumber = Common::GridMarker::BeatNumber::FirstDownBeat;

    DecimalNumber p_beatsPerMinute;

protected:
    // -- Overriden Common::MutableUtilityGridMarker Protected Instance Methods
    void p_setBeatsPerMinute(const DecimalNumber& beatsPerMinute) override
    {
        this->p_beatsPerMinute = beatsPerMinute;
    }

public:
    // -- Constructors & Destructors
    MutableUtilityGridMarker() = delete;
    MutableUtilityGridMarker(const DecimalNumber& positionInSeconds,
                             const DecimalNumber& beatsPerMinute) : p_positionInSeconds(positionInSeconds),
                                                                    p_beatsPerMinute(beatsPerMinute) { }
    template <class T>
        MutableUtilityGridMarker(T& other) : p_positionInSeconds(other.positionInSeconds()),
                                             p_flags(other.flags()),
                                             p_beatNumber(other.beatNumber()),
                                             p_beatsPerMinute(other.beatsPerMinute()) { }

    // -- Overriden Common::MutableUtilityGridMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_positionInSeconds;
    }
    void setPositionInSeconds(const DecimalNumber& positionInSeconds) override
    {
        this->p_positionInSeconds = positionInSeconds;
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_flags;
    }
    void setFlags(const Common::Marker::Flags& flags) override
    {
        this->p_flags = flags;
    }
    void setFlag(Common::Marker::Flag flag) override
    {
        this->p_flags.set(flag);
    }
    void clearFlag(Common::Marker::Flag flag) override
    {
        this->p_flags.clear(flag);
    }

    Optional<count> maybeHotCueNumber() const override
    {
        return this->p_maybeHotCueNumber;
    }
    void setHotCueNumber(const Optional<count>& maybeHotCueNumber) override
    {
        this->p_maybeHotCueNumber = maybeHotCueNumber;
    }

    BeatNumber beatNumber() const override
    {
        return this->p_beatNumber;
    }
    void setBeatNumber(Common::GridMarker::BeatNumber beatNumber) override
    {
        this->p_beatNumber = beatNumber;
    }

    DecimalNumber beatsPerMinute() const override
    {
        return this->p_beatsPerMinute;
    }
};

} }
