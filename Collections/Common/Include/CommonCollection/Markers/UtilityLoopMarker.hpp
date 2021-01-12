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

#include <CommonCollection/Markers/LoopMarker.hpp>

#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Flags.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class MutableUtilityLoopMarker : public Common::MutableLoopMarker
{
    // -- Private Instance Variables
    DecimalNumber p_positionInSeconds;
    DecimalNumber p_lengthInSeconds;

    Common::Marker::Flags p_flags;

    Optional<count> p_maybeHotCueNumber;

    Optional<String> p_maybeName;

    Optional<Color> p_maybeColor;

protected:
    // -- Overriden Common::MutableUtilityLoopMarker Protected Instance Variables
    void p_setName(const Optional<String>& maybeName) override
    {
        this->p_maybeName = maybeName;
    }
    void p_setColor(const Optional<Color>& maybeColor) override
    {
        this->p_maybeColor = maybeColor;
    }

public:
    // -- Constructors & Destructors
    MutableUtilityLoopMarker() = delete;
    MutableUtilityLoopMarker(const DecimalNumber& positionInSeconds,
                             const DecimalNumber& lengthInSeconds) : p_positionInSeconds{ positionInSeconds },
                                                                     p_lengthInSeconds{ lengthInSeconds } { }
    MutableUtilityLoopMarker(const DecimalNumber& positionInSeconds,
                             const DecimalNumber& lengthInSeconds,
                             count hotCueNumber) : p_positionInSeconds{ positionInSeconds },
                                                   p_lengthInSeconds{ lengthInSeconds },
                                                   p_maybeHotCueNumber{ hotCueNumber } { }
    template <class T>
        MutableUtilityLoopMarker(const T& other) : p_positionInSeconds{ other.positionInSeconds() },
                                                   p_lengthInSeconds{ other.lengthInSeconds() },
                                                   p_flags{ other.flags() },
                                                   p_maybeHotCueNumber{ other.maybeHotCueNumber() },
                                                   p_maybeName{ other.maybeName() },
                                                   p_maybeColor{ other.maybeColor() } { }

    // -- Overriden Common::MutableUtilityLoopMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_positionInSeconds;
    }
    void setPositionInSeconds(const DecimalNumber& positionInSeconds) override
    {
        this->p_positionInSeconds = positionInSeconds;
    }

    DecimalNumber lengthInSeconds() const override
    {
        return this->p_lengthInSeconds;
    }
    void setLengthInSeconds(const DecimalNumber& lengthInSeconds) override
    {
        this->p_lengthInSeconds = lengthInSeconds;
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

    Optional<String> maybeName() const override
    {
        return this->p_maybeName;
    }

    Optional<Color> maybeColor() const override
    {
        return this->p_maybeColor;
    }
};

} }
