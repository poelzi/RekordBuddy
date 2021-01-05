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

#include <TrackFiles/SeratoMarkers/SeratoMarkers.hpp>

using namespace NxA;

// -- Factory Methods

Optional<SeratoLoopMarker> SeratoLoopMarker::maybeMarkerWithMemoryAt(NotNull<const byte*> id3TagStart)
{
    auto tagStruct = reinterpret_cast<const SeratoLoopMarker::SeratoLoopTagV2Struct*>(id3TagStart.get());

    auto tagName = String{ reinterpret_cast<const character*>(tagStruct->tag) };
    if (tagName != "LOOP"_String) {
        return nothing;
    }

    return SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(tagStruct->label),
                                                                                Platform::bigEndianUInteger32ValueAt(tagStruct->position),
                                                                                Platform::bigEndianUInteger32ValueAt(tagStruct->loopPosition),
                                                                                Platform::bigEndianUInteger16ValueAt(tagStruct->index),
                                                                                Color{ tagStruct->color[1],
                                                                                       tagStruct->color[2],
                                                                                       tagStruct->color[3] });
}

Optional<SeratoLoopMarker> SeratoLoopMarker::maybeMarkerV1WithIndexAndRawMemoryAt(uinteger16 index, const byte* id3TagStart)
{
    auto tagStruct = reinterpret_cast<const SeratoMarker::SeratoRawTagV1Struct*>(id3TagStart);

    if (tagStruct->type != SeratoMarker::MarkerType::Loop) {
        return nothing;
    }

    return SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(String(),
                                                                                Platform::bigEndianUInteger32ValueAt(tagStruct->position),
                                                                                Platform::bigEndianUInteger32ValueAt(tagStruct->loopPosition),
                                                                                index,
                                                                                Color{ tagStruct->color[1],
                                                                                       tagStruct->color[2],
                                                                                       tagStruct->color[3] });
}

Optional<SeratoLoopMarker> SeratoLoopMarker::maybeMarkerV1WithIndexAndEncodedMemoryAt(uinteger16 index, const byte* id3TagStart)
{
    auto encodedStruct = reinterpret_cast<const SeratoMarker::SeratoEncodedTagV1Struct*>(id3TagStart);

    SeratoMarker::SeratoRawTagV1Struct rawStruct;
    SeratoMarker::rawV1TagFromEncodedV1TagStruct(rawStruct, *encodedStruct);

    return SeratoLoopMarker::maybeMarkerV1WithIndexAndRawMemoryAt(index, reinterpret_cast<const byte*>(&rawStruct));
}

Optional<SeratoLoopMarker> SeratoLoopMarker::maybeMarkerWithLabelStartEndPositionsIndexAndColor(const String& label,
                                                                                                uinteger32 startPositionInMilliseconds,
                                                                                                uinteger32 endPositionInMilliseconds,
                                                                                                uinteger16 index,
                                                                                                const Color& color)

{
    if (startPositionInMilliseconds >= endPositionInMilliseconds) {
        return nothing;
    }

    if (index > 7) {
        return nothing;
    }

    SeratoLoopMarker newMarker;
    newMarker.p_startPositionInMilliseconds = startPositionInMilliseconds;
    newMarker.p_endPositionInMilliseconds = endPositionInMilliseconds;
    newMarker.p_index = index;
    newMarker.p_label = label;

    // -- For the time being as far as I know, Serato loop markers do not have any colors even though the format supports it.
    newMarker.p_color = Color{ 0 };

    return { std::move(newMarker) };
}

// -- Class Methods

void SeratoLoopMarker::addEmptyRawMarkerV1TagTo(MutableBlob& data)
{
    // Note: Serato saves empty V1 loops with the 'loop' type (3) rather than empty (0)
    SeratoMarker::addRawMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Loop,
                                                0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
                                                Color{ 0 }, data);
}

void SeratoLoopMarker::addEmptyEncodedMarkerV1TagTo(MutableBlob& data)
{
    // Note: Serato saves empty V1 loops with the 'loop' type (3) rather than empty (0)
    SeratoMarker::addEncodedMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Loop,
                                                    0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,
                                                    Color{ 0 }, data);
}

bool SeratoLoopMarker::isValidV1RawMarker(const byte* tagStart)
{
    auto tagStruct = reinterpret_cast<const SeratoMarker::SeratoRawTagV1Struct*>(tagStart);
    return (Platform::bigEndianUInteger32ValueAt(tagStruct->position) != 0xffffffff);
}

bool SeratoLoopMarker::isValidV1EncodedMarker(const byte* tagStart)
{
    auto encodedStruct = reinterpret_cast<const SeratoMarker::SeratoEncodedTagV1Struct*>(tagStart);

    SeratoMarker::SeratoRawTagV1Struct rawStruct;
    SeratoMarker::rawV1TagFromEncodedV1TagStruct(rawStruct, *encodedStruct);

    return (Platform::bigEndianUInteger32ValueAt(rawStruct.position) != 0xffffffff);
}

integer32 SeratoLoopMarker::sizeOfV1RawMarker()
{
    return sizeof(SeratoMarker::SeratoRawTagV1Struct);
}

integer32 SeratoLoopMarker::sizeOfV1EncodedMarker()
{
    return sizeof(SeratoMarker::SeratoEncodedTagV1Struct);
}

// -- Instance Methods

void SeratoLoopMarker::addMarkerV2TagTo(MutableBlob& data) const
{
    SeratoLoopMarker::SeratoLoopTagV2Struct header;

    memcpy(header.tag, "LOOP", 5);
    count size = sizeof(SeratoLoopMarker::SeratoLoopTagV2Struct) + this->p_label.length() + 1 - sizeof(SeratoLoopMarker::SeratoLoopTagV2HeaderStruct);
    Platform::writeBigEndianUInteger32ValueAt(uint32_t(size), header.size);
    Platform::writeBigEndianUInteger16ValueAt(this->p_index, header.index);
    Platform::writeBigEndianUInteger32ValueAt(this->p_startPositionInMilliseconds, header.position);
    Platform::writeBigEndianUInteger32ValueAt(this->p_endPositionInMilliseconds, header.loopPosition);
    Platform::writeBigEndianUInteger32ValueAt(0xFFFFFFFF, header.loopIterations);
    header.color[0] = 0;
    header.color[1] = this->p_color.red();
    header.color[2] = this->p_color.green();
    header.color[3] = this->p_color.blue();
    header.loop_enabled = 0;
    header.loop_locked = 0;

    data.appendMemoryWithSize(reinterpret_cast<const byte*>(&header), sizeof(SeratoLoopMarker::SeratoLoopTagV2Struct));
    data.appendWithStringTermination(this->p_label.asUTF8());
}

void SeratoLoopMarker::addRawMarkerV1TagTo(MutableBlob& data) const
{
    SeratoMarker::addRawMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Loop,
                                                this->p_startPositionInMilliseconds,
                                                this->p_endPositionInMilliseconds,
                                                0xFFFFFFFF,
                                                this->p_color,
                                                data);
}

void SeratoLoopMarker::addEncodedMarkerV1TagTo(MutableBlob& data) const
{
    SeratoMarker::addEncodedMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Loop,
                                                    this->p_startPositionInMilliseconds,
                                                    this->p_endPositionInMilliseconds,
                                                    0xFFFFFFFF,
                                                    this->p_color,
                                                    data);
}
