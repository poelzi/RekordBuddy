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

#include <CommonCollection/Markers/UtilityGridMarker.hpp>

#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Flags.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/XMLNode.hpp>

namespace NxA { namespace Traktor {

// -- Forward Declarations
class MutableTrack;

// -- Public Interface
class GridMarker final : public Common::GridMarker
{
    // -- Friends
    friend class MutableGridMarker;
    friend class MutableTrack;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Variables
    Common::MutableUtilityGridMarker p_marker;

    const MutableTrack& p_fromTrack;

    // -- Protected Constructors & Destructors
    GridMarker(const DecimalNumber& positionInSeconds,
               const MutableTrack& fromTrack) : p_marker(positionInSeconds, DecimalNumber{ }), p_fromTrack{ fromTrack } { }

public:
    // -- Constructors & Destructors
    GridMarker() = delete;
    GridMarker(const Common::MutableUtilityGridMarker& marker, const MutableTrack& fromTrack, const Protected&) : p_marker{ marker }, p_fromTrack{ fromTrack } { }
    ~GridMarker() override = default;

    // -- Overriden Common::MutableGridMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_marker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        // -- There are currently no custom flags supported by Traktor.
        return { };
    }

    BeatNumber beatNumber() const override
    {
        return this->p_marker.beatNumber();
    }

    DecimalNumber beatsPerMinute() const override;
};

class MutableGridMarker final : public Common::MutableGridMarker
{
    // -- Friends
    friend class MutableTrack;

    // -- Private Instance Variables
    Traktor::GridMarker p_traktorMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Overridden Common::MutableGridMarker Instance Methods
    void p_setBeatsPerMinute(const DecimalNumber& bpm) override
    {
        // -- Traktor cannot accept bpm values that are not the same as the average beats per minute for the track.
    }

public:
    // -- Class Methods
    static void writeMarkerToNode(const Common::GridMarker& marker, MutableXMLNode& node)
    {
        static auto nnString = "n.n."_String;
        node.setStringValueForAttributeNamed(nnString, "NAME");

        static auto zeroString = "0"_String;
        node.setStringValueForAttributeNamed(zeroString, "DISPL_ORDER");

        static auto fourString = "4"_String;
        node.setStringValueForAttributeNamed(fourString, "TYPE");

        node.setStringValueForAttributeNamed((marker.positionInSeconds() * 1000).asStringWithFractionDigitsBetween(6, 6), "START");

        static auto zeroDecimalString = "0.000000"_String;
        node.setStringValueForAttributeNamed(zeroDecimalString, "LEN");

        static auto minusOneString = "-1"_String;
        node.setStringValueForAttributeNamed(minusOneString, "REPEATS");

        auto maybeHotCueNumber = marker.maybeHotCueNumber();
        node.setIntegerValueForAttributeNamed(maybeHotCueNumber.isValid() ? (*maybeHotCueNumber < 8) ? *maybeHotCueNumber : -1 : -1, "HOTCUE");
    }

    // -- Constructors & Destructors
    MutableGridMarker() = delete;
    MutableGridMarker(const DecimalNumber& positionInSeconds, const MutableTrack& fromTrack, const Protected&) : p_traktorMarker{ positionInSeconds, fromTrack } { }
    MutableGridMarker(const XMLNode& xmlNode,
                      const MutableTrack& fromTrack,
                      const Protected&) : p_traktorMarker{ xmlNode.maybeDecimalValueForAttributeNamed("START").valueOr(DecimalNumber{ }) / 1000,
                                                           fromTrack } { }
    ~MutableGridMarker() override = default;

    // -- Overridden Common::GridMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_traktorMarker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_traktorMarker.flags();
    }

    Common::GridMarker::BeatNumber beatNumber() const override
    {
        return this->p_traktorMarker.beatNumber();
    }

    DecimalNumber beatsPerMinute() const override
    {
        return this->p_traktorMarker.beatsPerMinute();
    }

    // -- Overridden Common::MutableGridMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_traktorMarker.p_marker.setPositionInSeconds(position);
    }

    void setFlag(Common::Marker::Flag) override
    {
        // -- There are currently no custom flags supported by Traktor.
    }
    void setFlags(const Common::Marker::Flags&) override
    {
        // -- There are currently no custom flags supported by Traktor.
    }
    void clearFlag(Common::Marker::Flag) override
    {
        // -- There are currently no custom flags supported by Traktor.
    }

    void setBeatNumber(Common::GridMarker::BeatNumber number) override
    {
        this->p_traktorMarker.p_marker.setBeatNumber(number);
    }

    // -- Instance Methods
    inline const Traktor::GridMarker& asImmutableReference() const
    {
        return this->p_traktorMarker;
    }

    void writeToNode(MutableXMLNode& node) const
    {
        MutableGridMarker::writeMarkerToNode(*this, node);
    }
};

} }
