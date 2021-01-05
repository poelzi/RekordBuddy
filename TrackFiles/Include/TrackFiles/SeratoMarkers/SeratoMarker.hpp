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

#include <Base/Base.hpp>

namespace NxA {

// -- Forward Declarations
class SeratoCueMarker;
class SeratoLoopMarker;
class SeratoGridMarker;

// -- Public Interface
class SeratoMarker
{
public:
    // -- Types
    using OfSomeSort = Variant<SeratoCueMarker, SeratoLoopMarker, SeratoGridMarker>;

    enum class MarkerType : byte {
        Empty = 0,
        Cue = 1,
        Loop = 3,
    };

    struct SeratoRawTagV1Struct {
        byte position[4];
        byte loopPosition[4];
        byte zero;
        byte loopIterations[4];
        byte color[4];
        MarkerType type;
        byte locked;
    };

    struct SeratoEncodedTagV1Struct {
        byte position[5];
        byte loopPosition[5];
        byte zero;
        byte loopIterations[5];
        byte color[4];
        MarkerType type;
        byte locked;
    };

    // -- Class Methods
    static boolean markersAreTheSameEvenIfInADifferentOrder(const MutableArray<SeratoMarker::OfSomeSort>&,
                                                            const MutableArray<SeratoMarker::OfSomeSort>&);

    static uinteger32 unpackUInteger32(const byte packed[5])
    {
        uinteger32 result = 0;
        result |= (packed[4] & 0x7f);
        result |= (packed[3] & 0x7f) << 7;
        result |= (packed[2] & 0x7f) << 14;
        result |= (packed[1] & 0x7f) << 21;
        result |= (packed[0] & 0x7f) << 28;
        return result;
    }

    static void unpackColor(const byte packed[4], byte& r, byte& g, byte& b)
    {
        r = ((packed[1] & 0x1f) >> 2) | ((packed[0] & 0x07) << 5);
        g = ((packed[2] & 0x3f) >> 1) | ((packed[1] & 0x03) << 6);
        b = ((packed[3] & 0x7f) >> 0) | ((packed[2] & 0x01) << 7);
    }

    static void packUInteger32(uinteger32 input, byte packed[5])
    {
        if (input == 0xFFFFFFFF) {
            packed[4] = packed[3] = packed[2] = packed[1] = packed[0] = 0x7f;
        }
        else {
            packed[4] = (input & 0x0000007f);
            packed[3] = (input & 0x00003f80) >> 7;
            packed[2] = (input & 0x001fc000) >> 14;
            packed[1] = (input & 0x0fe00000) >> 21;
            packed[0] = (input & 0xf0000000) >> 28;
        }
    }

    static void packColor(byte r, byte g, byte b, byte packed[4])
    {
        packed[3] = (b & 0x7f);
        packed[2] = ((b & 0x80) >> 7) | ((g & 0x3f) << 1);
        packed[1] = ((g & 0xc0) >> 6) | ((r & 0x1f) << 2);
        packed[0] = ((r & 0xe0) >> 5);
    }

    static void rawV1TagFromEncodedV1TagStruct(SeratoRawTagV1Struct& rawTag, const SeratoEncodedTagV1Struct& encodedTag)
    {
        integer32 position = unpackUInteger32(encodedTag.position);
        integer32 loopPosition = unpackUInteger32(encodedTag.loopPosition);
        integer32 loopIterations = unpackUInteger32(encodedTag.loopIterations);

        Platform::writeBigEndianUInteger32ValueAt(position, rawTag.position);
        Platform::writeBigEndianUInteger32ValueAt(loopPosition, rawTag.loopPosition);
        Platform::writeBigEndianUInteger32ValueAt(loopIterations, rawTag.loopIterations);
        rawTag.color[0] = 0;
        unpackColor(encodedTag.color, rawTag.color[1], rawTag.color[2], rawTag.color[3]);
        rawTag.locked = 0;
        rawTag.type = encodedTag.type;
        rawTag.zero = 0;
    }

    static void addRawMarkerV1TagWithFieldsTo(MarkerType type, integer32 position, integer32 loopPosition, integer32 loopIterations,
                                              const Color& color, MutableBlob& data)
    {
        SeratoRawTagV1Struct tag;
        Platform::writeBigEndianUInteger32ValueAt(position, tag.position);
        Platform::writeBigEndianUInteger32ValueAt(loopPosition, tag.loopPosition);
        Platform::writeBigEndianUInteger32ValueAt(loopIterations, tag.loopIterations);
        tag.color[0] = 0;
        tag.color[1] = color.red();
        tag.color[2] = color.green();
        tag.color[3] = color.blue();
        tag.locked = 0;
        tag.type = type;
        tag.zero = 0;

        data.append(Blob::withMemoryAndSize(reinterpret_cast<const byte*>(&tag), sizeof(SeratoRawTagV1Struct)));
    }

    static void addEncodedMarkerV1TagWithFieldsTo(MarkerType type, integer32 position, integer32 loopPosition, integer32 loopIterations,
                                                  const Color& color, MutableBlob& data)
    {
        SeratoEncodedTagV1Struct tag;
        packUInteger32(position, tag.position);
        packUInteger32(loopPosition, tag.loopPosition);
        packUInteger32(loopIterations, tag.loopIterations);
        packColor(color.red(), color.green(), color.blue(), tag.color);
        tag.locked = 0;
        tag.type = type;
        tag.zero = 0;

        data.append(Blob::withMemoryAndSize(reinterpret_cast<const byte*>(&tag), sizeof(SeratoEncodedTagV1Struct)));
    }
};

}
