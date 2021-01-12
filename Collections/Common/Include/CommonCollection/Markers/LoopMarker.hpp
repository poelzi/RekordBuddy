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
#include <Base/Color.hpp>
#include <Base/DecimalNumber.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Flags.hpp>

namespace NxA { namespace Common {

// -- Public Interface
class LoopMarker
{
public:
    // -- Constructors & Destructors
    virtual ~LoopMarker() = default;

    // -- Operators
    bool operator==(const LoopMarker& other) const
    {
        return (this->positionInSeconds() == other.positionInSeconds()) &&
               (this->lengthInSeconds() == other.lengthInSeconds()) &&
               (this->flags() == other.flags()) &&
               (this->maybeHotCueNumber() == other.maybeHotCueNumber()) &&
               (this->maybeName() == other.maybeName()) &&
               (this->maybeColor() == other.maybeColor());
    }
    inline bool operator!=(const LoopMarker& other) const
    {
        return !this->operator==(other);
    }

    // -- Instance Methods
    virtual DecimalNumber positionInSeconds() const = 0;
    virtual DecimalNumber lengthInSeconds() const = 0;

    virtual Common::Marker::Flags flags() const = 0;

    virtual Optional<count> maybeHotCueNumber() const = 0;

    virtual Optional<String> maybeName() const = 0;

    virtual Optional<Color> maybeColor() const = 0;

    String description() const
    {
        auto result = MutableString::stringWithFormat("LOOP at %s seconds with length of %s seconds",
                                                      this->positionInSeconds().asString().asUTF8(),
                                                      this->lengthInSeconds().asString().asUTF8());

        auto maybeName = this->maybeName();
        if (maybeName.isValid()) {
            result.appendStringWithFormat(", named '%s'", maybeName->asUTF8());
        }

        auto maybeHotCue = this->maybeHotCueNumber();
        if (maybeHotCue.isValid()) {
            result.appendStringWithFormat(", hotcue #%llu", *maybeHotCue);
        }

        auto maybeColor = this->maybeColor();
        if (maybeColor.isValid()) {
            result.appendStringWithFormat(", RGB Color 0x%06x", maybeColor->asRGB());
        }

        return { result };
    }
};

class MutableLoopMarker : public LoopMarker
{
protected:
    // -- Protected Instance Methods
    virtual void p_setColor(const Optional<Color>&) = 0;
    virtual void p_setName(const Optional<String>&) = 0;

public:
    // -- Constructors & Destructors
    virtual ~MutableLoopMarker() = default;

    // -- Instance Methods
    virtual void setPositionInSeconds(const DecimalNumber&) = 0;
    virtual void setLengthInSeconds(const DecimalNumber&) = 0;

    virtual void setFlag(Common::Marker::Flag) = 0;
    virtual void setFlags(const Common::Marker::Flags&) = 0;
    virtual void clearFlag(Common::Marker::Flag) = 0;

    virtual void setHotCueNumber(const Optional<count>&) = 0;

    void setName(const Optional<String>& maybeName)
    {
        // -- We don't set empty names
        NXA_ASSERT_TRUE(!maybeName.isValid() || (maybeName->length() > 0));

        this->p_setName(maybeName);
    }

    void setColor(const Optional<Color>& maybeColor)
    {
        // -- We don't set the color to black
        NXA_ASSERT_TRUE(!maybeColor.isValid() || (maybeColor->asRGB() != 0));

        this->p_setColor(maybeColor);
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
            this->setLengthInSeconds(other.lengthInSeconds());
            this->setFlags(other.flags());
            this->setHotCueNumber(other.maybeHotCueNumber());
            this->setName(other.maybeName());
            this->setColor(other.maybeColor());
        }
};

} }
