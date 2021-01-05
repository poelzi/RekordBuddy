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

#include <TrackFiles/SeratoMarkers/SeratoMarker.hpp>

#include <Base/Base.hpp>

namespace NxA {

// -- Public Interface
class SeratoCueMarker
{
    // -- Types
    struct SeratoCueTagV2HeaderStruct {
        byte tag[4];
        byte size[4];
    };

    struct SeratoCueTagV2Struct {
        byte tag[4];
        byte size[4];
        byte index[2];
        byte position[4];
        byte color[4];
        byte loop_enabled;
        byte loop_locked;
        character label[0];
    };

    // -- Private Instance Variables
    uinteger32 p_positionInMilliseconds;
    uinteger16 p_index;
    String p_label;
    Color p_color;

public:
    // -- Factory Methods
    static Optional<SeratoCueMarker> maybeMarkerWithMemoryAt(NotNull<const byte*>);
    static Optional<SeratoCueMarker> maybeMarkerV1WithIndexAndRawMemoryAt(uinteger16, const byte*);
    static Optional<SeratoCueMarker> maybeMarkerV1WithIndexAndEncodedMemoryAt(uinteger16, const byte*);
    static Optional<SeratoCueMarker> maybeMarkerWithLabelPositionIndexAndColor(const String&,
                                                                               uinteger32,
                                                                               uinteger16,
                                                                               const Color&);

    // -- Class Methods
    static String stringRepresentationForTimeInMilliseconds(uinteger32);

    static bool isValidV1RawMarker(const byte*);
    static bool isValidV1EncodedMarker(const byte*);
    static integer32 sizeOfV1RawMarker();
    static integer32 sizeOfV1EncodedMarker();
    static void addEmptyRawMarkerV1TagTo(MutableBlob&);
    static void addEmptyEncodedMarkerV1TagTo(MutableBlob&);

    // -- Constructors/Destructors
    SeratoCueMarker() : p_positionInMilliseconds{ 0 }, p_index{ 0 }, p_color{ 0 } { }
    ~SeratoCueMarker() = default;

    // -- Operators
    bool operator==(const SeratoCueMarker& other) const noexcept
    {
        return (this->p_positionInMilliseconds == other.p_positionInMilliseconds) &&
               (this->p_index == other.p_index) &&
               (this->p_color == other.p_color) &&
               (this->p_label == other.p_label);
    }
    bool operator!=(const SeratoCueMarker& other) const noexcept
    {
        return !this->operator==(other);
    }
    bool operator<(const SeratoCueMarker& other) const noexcept
    {
        return (this->p_positionInMilliseconds < other.p_positionInMilliseconds) ||
               (this->p_index < other.p_index) ||
               (this->p_color < other.p_color) ||
               (this->p_label < other.p_label);
    }

    // -- Instance Methods
    uinteger32 positionInMilliseconds() const
    {
        return this->p_positionInMilliseconds;
    }
    void setPositionInMilliseconds(uinteger32 position)
    {
        this->p_positionInMilliseconds = position;
    }

    uinteger16 index() const
    {
        return this->p_index;
    }
    void setIndex(uinteger16 index)
    {
        this->p_index = index;
    }

    String label() const
    {
        return this->p_label;
    }
    void setLabel(const String& label)
    {
        this->p_label = label;
    }

    Color color() const
    {
        return this->p_color;
    }
    void setColor(const Color& color)
    {
        this->p_color = color;
    }

    void addMarkerV2TagTo(MutableBlob&) const;
    void addRawMarkerV1TagTo(MutableBlob&) const;
    void addEncodedMarkerV1TagTo(MutableBlob&) const;
};

}
