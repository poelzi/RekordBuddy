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
#include <Base/XMLNode.hpp>

namespace NxA { namespace Rekordbox {

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
        // -- There are currently no custom flags supported by rekordbox.
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
    Rekordbox::GridMarker p_rekordboxMarker;

protected:
    // -- Protected Class Variables

    // -- This is used to make sure only friend or derived classes can call certain methods or constructors.
    // -- Making those methods or constructors protected or private would prevent things like Shared<> to
    // -- use them when being constructed themselves.
    constexpr inline static struct Protected { } p_isProtected = Protected{ };

    // -- Overridden Common::MutableGridMarker Instance Methods
    void p_setBeatsPerMinute(const DecimalNumber& bpm) override
    {
        this->p_rekordboxMarker.p_marker.setBeatsPerMinute(bpm);
    }

public:
    // -- Constructors & Destructors
    MutableGridMarker() = delete;
    MutableGridMarker(const DecimalNumber& positionInSeconds,
                      const DecimalNumber& beatsPerMinute,
                      const Protected&) : p_rekordboxMarker{ positionInSeconds, beatsPerMinute } { }
    MutableGridMarker(MutableXMLNode xmlNode,
                      const Protected&) : p_rekordboxMarker{ xmlNode.maybeDecimalValueForAttributeNamed("Inizio").valueOr(DecimalNumber{ }),
                                                             xmlNode.maybeDecimalValueForAttributeNamed("Bpm").valueOr(DecimalNumber::withInteger(123)) }
    {
        Common::GridMarker::BeatNumber beatNumber = BeatNumber::FirstDownBeat;

        auto maybeBeatNumber = xmlNode.maybeCountValueForAttributeNamed("Battito");
        if (maybeBeatNumber.isValid()) {
            switch (*maybeBeatNumber) {
                case 2: {
                    beatNumber = BeatNumber::SecondDownBeat;
                    break;
                }
                case 3: {
                    beatNumber = BeatNumber::ThirdDownBeat;
                    break;
                }
                case 4: {
                    beatNumber = BeatNumber::FourthDownBeat;
                    break;
                }
            }
        }

        this->p_rekordboxMarker.p_marker.setBeatNumber(beatNumber);
    }
    ~MutableGridMarker() override = default;

    // -- Overridden Common::GridMarker Instance Methods
    DecimalNumber positionInSeconds() const override
    {
        return this->p_rekordboxMarker.positionInSeconds();
    }

    Common::Marker::Flags flags() const override
    {
        return this->p_rekordboxMarker.flags();
    }

    Common::GridMarker::BeatNumber beatNumber() const override
    {
        return this->p_rekordboxMarker.beatNumber();
    }

    DecimalNumber beatsPerMinute() const override
    {
        return this->p_rekordboxMarker.beatsPerMinute();
    }

    // -- Overridden Common::MutableGridMarker Instance Methods
    void setPositionInSeconds(const DecimalNumber& position) override
    {
        this->p_rekordboxMarker.p_marker.setPositionInSeconds(position);
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

    void setBeatNumber(Common::GridMarker::BeatNumber number) override
    {
        this->p_rekordboxMarker.p_marker.setBeatNumber(number);
    }

    // -- Instance Methods
    inline const Rekordbox::GridMarker& asImmutableReference() const
    {
        return this->p_rekordboxMarker;
    }

    void writeToTrackNode(MutableXMLNode& node) const
    {
        auto newNode = node.appendSubNodeNamed("TEMPO");
        newNode.setStringValueForAttributeNamed(this->positionInSeconds().asStringWithFractionDigitsBetween(3, 3), "Inizio");
        newNode.setStringValueForAttributeNamed(this->beatsPerMinute().asStringWithFractionDigitsBetween(2, 2), "Bpm");

        static auto metroString = "4/4"_String;
        newNode.setStringValueForAttributeNamed(metroString, "Metro");

        count beatNumber;
        switch (this->beatNumber()) {
            case Common::GridMarker::BeatNumber::FirstDownBeat: {
                beatNumber = 1;
                break;
            }
            case Common::GridMarker::BeatNumber::SecondDownBeat: {
                beatNumber = 2;
                break;
            }
            case Common::GridMarker::BeatNumber::ThirdDownBeat: {
                beatNumber = 3;
                break;
            }
            case Common::GridMarker::BeatNumber::FourthDownBeat: {
                beatNumber = 4;
                break;
            }
        }

        newNode.setCountValueForAttributeNamed(beatNumber, "Battito");
    }
};

} }
