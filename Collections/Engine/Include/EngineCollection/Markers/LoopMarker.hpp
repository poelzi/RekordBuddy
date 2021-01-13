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

#include <CommonCollection/Markers/UtilityLoopMarker.hpp>
#include <CommonCollection/Markers/MarkerColor.hpp>

#include <CommonCollection/Markers/LoopMarker.hpp>

#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Flags.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>

namespace NxA { namespace Engine {

// -- Public Interface
class LoopMarker : public Common::LoopMarker
{
    // -- Friends
    friend class MutableLoopMarker;
    friend class MutableTrack;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Variables
    Common::MutableUtilityLoopMarker p_marker;

public:
    // -- Constructors & Destructors
    LoopMarker() = delete;
    LoopMarker(const DecimalNumber& positionInSeconds,
               const DecimalNumber& lengthInSeconds) : p_marker{ positionInSeconds, lengthInSeconds } { }
    ~LoopMarker() override = default;

    // -- Overriden Common::LoopMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_marker.positionInSeconds();
    }

    DecimalNumber lengthInSeconds() const override
    {
        return this->p_marker.lengthInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        // -- There are currently no custom flags supported.
        return { };
    }

    Optional<count> maybeHotCueNumber() const override
    {
        return this->p_marker.maybeHotCueNumber();
    }

    Optional<String> maybeName() const override
    {
        return this->p_marker.maybeName();
    }

    Optional<Color> maybeColor() const override
    {
        return this->p_marker.maybeColor();
    }
};

class MutableLoopMarker final : public Common::MutableLoopMarker
{
    // -- Friends
    friend class MutableTrack;

    // -- Private Instance Variables
    Engine::LoopMarker p_internalMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Methods
    void p_setName(const Optional<String>& maybeName) override
    {
        this->p_internalMarker.p_marker.setName(maybeName);
    }
    void p_setColor(const Optional<Color>& maybeColor) override
    {
        this->p_internalMarker.p_marker.setColor(maybeColor);
    }

public:
    // -- Constructors & Destructors
    MutableLoopMarker() = delete;
    MutableLoopMarker(const DecimalNumber& positionInSeconds,
                      const DecimalNumber& lengthInSeconds,
                      const Protected&) : p_internalMarker{ positionInSeconds, lengthInSeconds } { }
    ~MutableLoopMarker() override = default;

    // -- Overridden Common::LoopMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_internalMarker.positionInSeconds();
    }
    DecimalNumber lengthInSeconds() const override
    {
        return this->p_internalMarker.lengthInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_internalMarker.flags();
    }

    Optional<count> maybeHotCueNumber() const override
    {
        return this->p_internalMarker.maybeHotCueNumber();
    }

    Optional<String> maybeName() const override
    {
        return this->p_internalMarker.maybeName();
    }

    Optional<Color> maybeColor() const override
    {
        return this->p_internalMarker.maybeColor();
    }

    // -- Overridden Common::MutableLoopMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_internalMarker.p_marker.setPositionInSeconds(position);
    }
    void setLengthInSeconds(const DecimalNumber& length) override
    {
        this->p_internalMarker.p_marker.setLengthInSeconds(length);
    }

    void setFlag(Common::Marker::Flag) override
    {
        // -- There are currently no custom flags supported.
    }
    void setFlags(const Common::Marker::Flags&) override
    {
        // -- There are currently no custom flags supported.
    }
    void clearFlag(Common::Marker::Flag) override
    {
        // -- There are currently no custom flags supported.
    }

    void setHotCueNumber(const Optional<count>& maybeHotCueNumber) override
    {
        if (maybeHotCueNumber.isValid()) {
            auto hotCueValue = *maybeHotCueNumber;
            if ((hotCueValue >= 0) && (hotCueValue <= 8)) {
                this->p_internalMarker.p_marker.setHotCueNumber(maybeHotCueNumber);
                return;
            }
        }

        this->p_internalMarker.p_marker.setHotCueNumber(nothing);
    }
};

} }
