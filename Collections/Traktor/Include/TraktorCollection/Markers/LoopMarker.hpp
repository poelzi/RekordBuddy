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

#include <CommonCollection/Markers/UtilityLoopMarker.hpp>
#include <CommonCollection/Markers/MarkerColor.hpp>

#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Flags.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/XMLNode.hpp>

namespace NxA { namespace Traktor {

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

    // -- Protected Constructors & Destructors
    LoopMarker(const DecimalNumber& positionInSeconds,
               const DecimalNumber& lengthInSeconds) : p_marker(positionInSeconds, lengthInSeconds) { }

public:
    // -- Constructors & Destructors
    LoopMarker() = delete;
    ~LoopMarker() override = default;

    // -- Overriden Common::MutableLoopMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_marker.positionInSeconds();
    }

    DecimalNumber lengthInSeconds() const override
    {
        return this->p_marker.lengthInSeconds();
    }

    // -- There are currently no custom flags supported by Traktor.
    Common::Marker::Flags flags() const override
    {
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
        return Common::MarkerColor::GreenColor;
    }
};

class MutableLoopMarker final : public Common::MutableLoopMarker
{
    // -- Friends
    friend class MutableTrack;

    // -- Private Instance Variables
    Traktor::LoopMarker p_traktorMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Methods
    void p_setName(const Optional<String>& maybeName) override
    {
        this->p_traktorMarker.p_marker.setName(maybeName);
    }
    void p_setColor(const Optional<Color>& maybeColor) override
    {
        // -- Traktor marker colors are determined by their type, and not user selectable.
    }

public:
    // -- Class Methods
    static void writeMarkerToNode(const Common::LoopMarker& marker, MutableXMLNode& node)
    {
        auto maybeName = marker.maybeName();
        node.setStringValueForAttributeNamed(maybeName.isValid() ? *maybeName : ""_String, "NAME");

        static auto zeroString = "0"_String;
        node.setStringValueForAttributeNamed(zeroString, "DISPL_ORDER");

        static auto fiveString = "5"_String;
        node.setStringValueForAttributeNamed(fiveString, "TYPE");
        node.setStringValueForAttributeNamed((marker.positionInSeconds() * 1000).asStringWithFractionDigitsBetween(6, 6), "START");
        node.setStringValueForAttributeNamed((marker.lengthInSeconds() * 1000).asStringWithFractionDigitsBetween(6, 6), "LEN");

        static auto minusOneString = "-1"_String;
        node.setStringValueForAttributeNamed(minusOneString, "REPEATS");

        auto maybeHotCueIndex = marker.maybeHotCueNumber();
        node.setIntegerValueForAttributeNamed(maybeHotCueIndex.isValid() ? (*maybeHotCueIndex < 8) ? *maybeHotCueIndex : -1 : -1, "HOTCUE");
    }

    // -- Constructors & Destructors
    MutableLoopMarker() = delete;
    MutableLoopMarker(const DecimalNumber& positionInSeconds, const Protected&) : p_traktorMarker{ positionInSeconds, DecimalNumber::withInteger(1) } { }
    MutableLoopMarker(const XMLNode& xmlNode, const Protected&) : p_traktorMarker{ xmlNode.maybeDecimalValueForAttributeNamed("START").valueOr(DecimalNumber{ }) / 1000,
                                                                                   xmlNode.maybeDecimalValueForAttributeNamed("LEN").valueOr(DecimalNumber::withInteger(1000)) / 1000}
    {
        auto maybeHotCue = xmlNode.maybeIntegerValueForAttributeNamed("HOTCUE");
        auto hotCueValue = maybeHotCue.valueOr(-1);
        if ((hotCueValue >= 0) && (hotCueValue <= 8)) {
            this->p_traktorMarker.p_marker.setHotCueNumber(*maybeHotCue);
        }

        auto maybeName = xmlNode.maybeStringValueForAttributeNamed("NAME");
        if (maybeName.isValid() && (maybeName->length() > 0)) {
            this->p_traktorMarker.p_marker.setName(maybeName);
        }
    }
    ~MutableLoopMarker() override = default;

    // -- Overridden Common::LoopMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_traktorMarker.positionInSeconds();
    }
    DecimalNumber lengthInSeconds() const override
    {
        return this->p_traktorMarker.lengthInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        // -- There are currently no custom flags supported by Traktor.
        return { };
    }

    Optional<count> maybeHotCueNumber() const override
    {
        return this->p_traktorMarker.maybeHotCueNumber();
    }

    Optional<String> maybeName() const override
    {
        return this->p_traktorMarker.maybeName();
    }

    Optional<Color> maybeColor() const override
    {
        return this->p_traktorMarker.maybeColor();
    }

    // -- Overridden Common::MutableLoopMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_traktorMarker.p_marker.setPositionInSeconds(position);
    }
    void setLengthInSeconds(const DecimalNumber& length) override
    {
        this->p_traktorMarker.p_marker.setLengthInSeconds(length);
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

    void setHotCueNumber(const Optional<count>& maybeHotCueNumber) override
    {
        if (maybeHotCueNumber.isValid()) {
            auto hotCueValue = *maybeHotCueNumber;
            if ((hotCueValue >= 0) && (hotCueValue <= 8)) {
                this->p_traktorMarker.p_marker.setHotCueNumber(maybeHotCueNumber);
                return;
            }
        }

        this->p_traktorMarker.p_marker.setHotCueNumber(nothing);
    }

    // -- Instance Methods
    inline const Traktor::LoopMarker& asImmutableReference() const
    {
        return this->p_traktorMarker;
    }

    void writeToNode(MutableXMLNode& node) const
    {
        MutableLoopMarker::writeMarkerToNode(*this, node);
    }
};

} }
