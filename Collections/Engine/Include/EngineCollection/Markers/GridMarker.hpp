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

#include <CommonCollection/Markers/UtilityGridMarker.hpp>
#include <CommonCollection/Markers/GridMarker.hpp>

#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Flags.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>

namespace NxA { namespace Engine {

// -- Public Interface
class GridMarker final : public Common::GridMarker
{
    // -- Friends
    friend class MutableGridMarker;
    friend class MutableTrack;

    // -- Protected Instance Variables
    Common::MutableUtilityGridMarker p_marker;

public:
    // -- Constructors & Destructors
    GridMarker() = delete;
    GridMarker(const DecimalNumber& positionInSeconds,
               const DecimalNumber& beatsPerMinute) : p_marker{ positionInSeconds, beatsPerMinute } { }
    ~GridMarker() override = default;

    // -- Overriden Common::GridMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_marker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        // -- There are currently no custom flags supported.
        return { };
    }

    BeatNumber beatNumber() const override
    {
        return this->p_marker.beatNumber();
    }

    DecimalNumber beatsPerMinute() const override
    {
        return this->p_marker.beatsPerMinute();
    }
};

class MutableGridMarker final : public Common::MutableGridMarker
{
    // -- Friends
    friend class MutableTrack;

    // -- Private Instance Variables
    Engine::GridMarker p_internalMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Overridden Common::MutableGridMarker Instance Methods
    void p_setBeatsPerMinute(const DecimalNumber& bpm) override
    {
        this->p_internalMarker.p_marker.setBeatsPerMinute(bpm);
    }

public:
    // -- Constructors & Destructors
    MutableGridMarker() = delete;
    MutableGridMarker(const DecimalNumber& positionInSeconds,
                      const DecimalNumber& beatsPerMinute,
                      const Protected&) : p_internalMarker{ positionInSeconds, beatsPerMinute } { }
    ~MutableGridMarker() override = default;

    // -- Overridden Common::GridMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_internalMarker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_internalMarker.flags();
    }

    Common::GridMarker::BeatNumber beatNumber() const override
    {
        return this->p_internalMarker.beatNumber();
    }

    DecimalNumber beatsPerMinute() const override
    {
        return this->p_internalMarker.beatsPerMinute();
    }

    // -- Overridden Common::MutableGridMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_internalMarker.p_marker.setPositionInSeconds(position);
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

    void setBeatNumber(Common::GridMarker::BeatNumber number) override
    {
        this->p_internalMarker.p_marker.setBeatNumber(number);
    }
};

} }
