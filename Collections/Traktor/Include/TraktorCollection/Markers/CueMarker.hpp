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

#include <CommonCollection/Markers/CueMarker.hpp>

#include <CommonCollection/Markers/UtilityCueMarker.hpp>
#include <CommonCollection/Markers/MarkerColor.hpp>

#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Flags.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/XMLNode.hpp>

namespace NxA { namespace Traktor {

// -- Public Interface
class CueMarker final : public Common::CueMarker
{
    // -- Friends
    friend class MutableCueMarker;
    friend class MutableTrack;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Variables
    Common::MutableUtilityCueMarker p_marker;

    // -- Protected Constructors & Destructors
    CueMarker(const DecimalNumber& positionInSeconds) : p_marker(positionInSeconds) { }

public:
    // -- Constructors & Destructors
    CueMarker() = delete;
    ~CueMarker() override = default;

    // -- Overriden Common::CueMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_marker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_marker.flags();
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
        if (this->p_marker.flags().has(Common::Marker::Flag::IsAFadeInMarker) ||
            this->p_marker.flags().has(Common::Marker::Flag::IsAFadeOutMarker)) {
            return Common::MarkerColor::OrangeColor;
        }
        else if (this->p_marker.flags().has(Common::Marker::Flag::IsALoadMarker)) {
            return Common::MarkerColor::YellowColor;
        }

        return Common::MarkerColor::TurquoiseColor;
    }
};

class MutableCueMarker final : public Common::MutableCueMarker
{
    // -- Friends
    friend class MutableTrack;

    // -- Private Instance Variables
    Traktor::CueMarker p_traktorMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Overridden Common::MutableCueMarker Instance Methods
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
    static void writeMarkerToNode(const Common::CueMarker& marker, MutableXMLNode& node)
    {
        auto maybeName = marker.maybeName();
        node.setStringValueForAttributeNamed(maybeName.isValid() ? *maybeName : ""_String, "NAME");

        static auto zeroString = "0"_String;
        node.setStringValueForAttributeNamed(zeroString, "DISPL_ORDER");

        static auto CueType = "0"_String;
        static auto FadeInType = "1"_String;
        static auto FadeOutType = "2"_String;
        static auto LoadType = "3"_String;

        String* type;
        if (marker.flags().has(Common::Marker::Flag::IsAFadeInMarker)) {
            type = &FadeInType;
        }
        else if (marker.flags().has(Common::Marker::Flag::IsAFadeOutMarker)) {
            type = &FadeOutType;
        }
        else if (marker.flags().has(Common::Marker::Flag::IsALoadMarker)) {
            type = &LoadType;
        }
        else {
            type = &CueType;
        }

        node.setStringValueForAttributeNamed(*type, "TYPE");
        node.setStringValueForAttributeNamed((marker.positionInSeconds() * 1000).asStringWithFractionDigitsBetween(6, 6), "START");

        static auto zeroDecimalString = "0.000000"_String;
        node.setStringValueForAttributeNamed(zeroDecimalString, "LEN");

        static auto minusOneString = "-1"_String;
        node.setStringValueForAttributeNamed(minusOneString , "REPEATS");

        auto maybeHotCueNumber = marker.maybeHotCueNumber();
        node.setIntegerValueForAttributeNamed(maybeHotCueNumber.isValid() ? (*maybeHotCueNumber < 8) ? *maybeHotCueNumber : -1 : -1, "HOTCUE");
    }

    // -- Constructors & Destructors
    MutableCueMarker() = delete;
    MutableCueMarker(const DecimalNumber& positionInSeconds, const Protected&) : p_traktorMarker{ positionInSeconds } { }
    MutableCueMarker(const XMLNode& xmlNode, const Protected&) : p_traktorMarker{ xmlNode.maybeDecimalValueForAttributeNamed("START").valueOr(DecimalNumber{ }) / 1000 }
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

        auto maybeType = xmlNode.maybeCountValueForAttributeNamed("TYPE");
        if (maybeType.isValid()) {
            switch (*maybeType) {
                case 1:
                    // -- "Fade-in"
                    this->p_traktorMarker.p_marker.setFlags(Common::Marker::Flag::IsAFadeInMarker);
                    break;
                case 2: {
                    // -- "Fade-out"
                    this->p_traktorMarker.p_marker.setFlags(Common::Marker::Flag::IsAFadeOutMarker);
                    break;
                }
                case 3: {
                    // -- "Load"
                    this->p_traktorMarker.p_marker.setFlags(Common::Marker::Flag::IsALoadMarker);
                    break;
                }
            }
        }
    }
    ~MutableCueMarker() override = default;

    // -- Overridden Common::CueMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_traktorMarker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_traktorMarker.flags();
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

    // -- Overridden Common::MutableCueMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_traktorMarker.p_marker.setPositionInSeconds(position);
    }

    void setFlag(Common::Marker::Flag flag) override
    {
        this->p_traktorMarker.p_marker.setFlag(flag);
    }
    void setFlags(const Common::Marker::Flags& flags) override
    {
        this->p_traktorMarker.p_marker.setFlags(flags);
    }
    void clearFlag(Common::Marker::Flag flag) override
    {
        this->p_traktorMarker.p_marker.clearFlag(flag);
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
    inline const Traktor::CueMarker& asImmutableReference() const
    {
        return this->p_traktorMarker;
    }

    void writeToNode(MutableXMLNode& node) const
    {
        MutableCueMarker::writeMarkerToNode(*this, node);
    }
};

} }
