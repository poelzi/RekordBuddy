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
#include <Base/XMLNode.hpp>

namespace NxA { namespace Rekordbox {

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
        // -- There are currently no custom flags supported by rekordbox.
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
        // -- rekordbox doesn't support colors for memory cues.
        return this->maybeHotCueNumber().isValid() ? this->p_marker.maybeColor() : Optional<Color> { };
    }
};

class MutableLoopMarker final : public Common::MutableLoopMarker
{
    // -- Friends
    friend class MutableTrack;

    // -- Private Instance Variables
    Rekordbox::LoopMarker p_rekordboxMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Protected Instance Methods
    void p_setName(const Optional<String>& maybeName) override
    {
        this->p_rekordboxMarker.p_marker.setName(maybeName);
    }
    void p_setColor(const Optional<Color>& maybeColor) override
    {
        this->p_rekordboxMarker.p_marker.setColor(maybeColor.isValid() ? Common::MarkerColor::maybeColorForMarkerColorAndMaybeHotCueNumberWhenExportedTo(*maybeColor,
                                                                                                                                                          nothing,
                                                                                                                                                          Common::Collection::Type::rekordbox,
                                                                                                                                                          Common::MarkerColor::IsALoopMarker::Yes)
                                                                       : Optional<Color>{ });
    }

public:
    // -- Constructors & Destructors
    MutableLoopMarker() = delete;
    MutableLoopMarker(const DecimalNumber& positionInSeconds,
                      const DecimalNumber& lengthInSeconds,
                      const Protected&) : p_rekordboxMarker{ positionInSeconds, lengthInSeconds } { }
    MutableLoopMarker(const XMLNode& xmlNode,
                      const Protected&) : p_rekordboxMarker{ xmlNode.maybeDecimalValueForAttributeNamed("Start").valueOr(DecimalNumber{ }),
                                                             DecimalNumber::withInteger(1) }
    {
        auto maybeEnd = xmlNode.maybeDecimalValueForAttributeNamed("End");
        if (maybeEnd.isValid()) {
            this->p_rekordboxMarker.p_marker.setLengthInSeconds(*maybeEnd - this->p_rekordboxMarker.positionInSeconds());
        }

        auto maybeHotCue = xmlNode.maybeIntegerValueForAttributeNamed("Num");
        auto hotCueValue = maybeHotCue.valueOr(-1);
        if ((hotCueValue >= 0) && (hotCueValue <= 8)) {
            this->p_rekordboxMarker.p_marker.setHotCueNumber(*maybeHotCue);

            // -- rekordbox doesn't support colors for memory cues.
            auto maybeRed = xmlNode.maybeCountValueForAttributeNamed("Red");
            if (maybeRed.isValid() && (*maybeRed <= 255)) {
                auto maybeGreen = xmlNode.maybeCountValueForAttributeNamed("Green");
                if (maybeGreen.isValid() && (*maybeGreen <= 255)) {
                    auto maybeBlue = xmlNode.maybeCountValueForAttributeNamed("Blue");
                    if (maybeBlue.isValid() && (*maybeBlue <= 255)) {
                        this->p_rekordboxMarker.p_marker.setColor(Color{ static_cast<byte>(*maybeRed),
                                                                         static_cast<byte>(*maybeGreen),
                                                                         static_cast<byte>(*maybeBlue) });
                    }
                }
            }
        }

        auto maybeName = xmlNode.maybeStringValueForAttributeNamed("Name");
        if (maybeName.isValid() && (maybeName->length() > 0)) {
            this->p_rekordboxMarker.p_marker.setName(maybeName);
        }
    }
    ~MutableLoopMarker() override = default;

    // -- Overridden Common::LoopMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_rekordboxMarker.positionInSeconds();
    }
    DecimalNumber lengthInSeconds() const override
    {
        return this->p_rekordboxMarker.lengthInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_rekordboxMarker.flags();
    }

    Optional<count> maybeHotCueNumber() const override
    {
        return this->p_rekordboxMarker.maybeHotCueNumber();
    }

    Optional<String> maybeName() const override
    {
        return this->p_rekordboxMarker.maybeName();
    }

    Optional<Color> maybeColor() const override
    {
        return this->p_rekordboxMarker.maybeColor();
    }

    // -- Overridden Common::MutableLoopMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_rekordboxMarker.p_marker.setPositionInSeconds(position);
    }
    void setLengthInSeconds(const DecimalNumber& length) override
    {
        this->p_rekordboxMarker.p_marker.setLengthInSeconds(length);
    }

    void setFlag(Common::Marker::Flag) override
    {
        // -- There are currently no custom flags supported by rekordbox.
    }
    void setFlags(const Common::Marker::Flags&) override
    {
        // -- There are currently no custom flags supported by rekordbox.
    }
    void clearFlag(Common::Marker::Flag) override
    {
        // -- There are currently no custom flags supported by rekordbox.
    }

    void setHotCueNumber(const Optional<count>& maybeHotCueNumber) override
    {
        if (maybeHotCueNumber.isValid()) {
            auto hotCueValue = *maybeHotCueNumber;
            if ((hotCueValue >= 0) && (hotCueValue <= 8)) {
                this->p_rekordboxMarker.p_marker.setHotCueNumber(maybeHotCueNumber);
                return;
            }
        }

        this->p_rekordboxMarker.p_marker.setHotCueNumber(nothing);
    }

    // -- Instance Methods
    inline const Rekordbox::LoopMarker& asImmutableReference() const
    {
        return this->p_rekordboxMarker;
    }

    void writeToTrackNode(MutableXMLNode& node) const
    {
        auto newNode = node.appendSubNodeNamed("POSITION_MARK");

        newNode.setStringValueForAttributeNamed(this->maybeName().valueOr(String{ }), "Name");

        static auto typeString = "4"_String;
        newNode.setStringValueForAttributeNamed(typeString, "Type");

        auto position = this->positionInSeconds();
        newNode.setStringValueForAttributeNamed(position.asStringWithFractionDigitsBetween(3, 3), "Start");
        newNode.setStringValueForAttributeNamed((this->lengthInSeconds() + position).asStringWithFractionDigitsBetween(3, 3), "End");

        auto maybeHotCueNumber = this->maybeHotCueNumber();
        newNode.setIntegerValueForAttributeNamed(maybeHotCueNumber.isValid() ? (*maybeHotCueNumber < 8) ? *maybeHotCueNumber : -1 : -1, "Num");

        if (maybeHotCueNumber.isValid()) {
            // -- rekordbox doesn't support colors for memory cues.
            auto maybeColor = this->maybeColor();
            if (maybeColor.isValid()) {
                newNode.setCountValueForAttributeNamed(maybeColor->red(), "Red");
                newNode.setCountValueForAttributeNamed(maybeColor->green(), "Green");
                newNode.setCountValueForAttributeNamed(maybeColor->blue(), "Blue");
            }
        }
    }
};

} }
