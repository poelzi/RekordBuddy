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

Optional<SeratoCueMarker> SeratoCueMarker::maybeMarkerWithMemoryAt(NotNull<const byte*> id3TagStart)
{
    auto tagStruct = reinterpret_cast<const SeratoCueMarker::SeratoCueTagV2Struct*>(id3TagStart.get());

    auto tagName = String{ reinterpret_cast<const character*>(tagStruct->tag) };
    if (tagName != "CUE"_String) {
        return nothing;
    }

    return SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(tagStruct->label),
                                                                      Platform::bigEndianUInteger32ValueAt(tagStruct->position),
                                                                      Platform::bigEndianUInteger16ValueAt(tagStruct->index),
                                                                      Color{ tagStruct->color[1],
                                                                             tagStruct->color[2],
                                                                             tagStruct->color[3] });
}

Optional<SeratoCueMarker> SeratoCueMarker::maybeMarkerV1WithIndexAndRawMemoryAt(uinteger16 index, const byte* id3TagStart)
{
    auto tagStruct = reinterpret_cast<const SeratoMarker::SeratoRawTagV1Struct*>(id3TagStart);

    if (tagStruct->type != SeratoMarker::MarkerType::Cue) {
        return nothing;
    }

    return SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(String(),
                                                                      Platform::bigEndianUInteger32ValueAt(tagStruct->position),
                                                                      index,
                                                                      Color{ tagStruct->color[1],
                                                                             tagStruct->color[2],
                                                                             tagStruct->color[3] });
}

Optional<SeratoCueMarker> SeratoCueMarker::maybeMarkerV1WithIndexAndEncodedMemoryAt(uinteger16 index, const byte* id3TagStart)
{
    auto encodedStruct = reinterpret_cast<const SeratoMarker::SeratoEncodedTagV1Struct*>(id3TagStart);

    SeratoMarker::SeratoRawTagV1Struct rawStruct;
    SeratoMarker::rawV1TagFromEncodedV1TagStruct(rawStruct, *encodedStruct);

    return SeratoCueMarker::maybeMarkerV1WithIndexAndRawMemoryAt(index, reinterpret_cast<const byte*>(&rawStruct));
}

Optional<SeratoCueMarker> SeratoCueMarker::maybeMarkerWithLabelPositionIndexAndColor(const String& label,
                                                                                     uinteger32 positionInMilliseconds,
                                                                                     uinteger16 index,
                                                                                     const Color& color)
{
    if (index > 7) {
        return nothing;
    }

    SeratoCueMarker newMarker;
    newMarker.p_positionInMilliseconds = positionInMilliseconds;
    newMarker.p_index = index;
    newMarker.p_label = label;
    newMarker.p_color = color;

    return { std::move(newMarker) };
}

// -- Class Methods

String SeratoCueMarker::stringRepresentationForTimeInMilliseconds(uinteger32 timeInMilliseconds)
{
    uinteger32 seconds = timeInMilliseconds / 1000;
    uinteger32 minutes = seconds / 60;
    uinteger32 hours = minutes / 60;

    timeInMilliseconds %= 1000;
    seconds %= 60;

    if (hours) {
        minutes %= 60;

        return String::stringWithFormat("%02ld:%02ld:%02ld:%03ld", hours, minutes, seconds, timeInMilliseconds);
    }
    else {
        return String::stringWithFormat("%02ld:%02ld:%03ld", minutes, seconds, timeInMilliseconds);
    }
}

bool SeratoCueMarker::isValidV1RawMarker(const byte* tagStart)
{
    auto tagStruct = reinterpret_cast<const SeratoMarker::SeratoRawTagV1Struct*>(tagStart);
    return (Platform::bigEndianUInteger32ValueAt(tagStruct->position) != 0xffffffff);
}

bool SeratoCueMarker::isValidV1EncodedMarker(const byte* tagStart)
{
    auto encodedStruct = reinterpret_cast<const SeratoMarker::SeratoEncodedTagV1Struct*>(tagStart);

    SeratoMarker::SeratoRawTagV1Struct rawStruct;
    SeratoMarker::rawV1TagFromEncodedV1TagStruct(rawStruct, *encodedStruct);

    return (Platform::bigEndianUInteger32ValueAt(rawStruct.position) != 0xffffffff);
}

integer32 SeratoCueMarker::sizeOfV1RawMarker()
{
    return sizeof(SeratoMarker::SeratoRawTagV1Struct);
}

integer32 SeratoCueMarker::sizeOfV1EncodedMarker()
{
    return sizeof(SeratoMarker::SeratoEncodedTagV1Struct);
}

void SeratoCueMarker::addEmptyRawMarkerV1TagTo(MutableBlob& data)
{
    SeratoMarker::addRawMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Empty,
                                                0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu,
                                                Color{ 0 }, data);
}

void SeratoCueMarker::addEmptyEncodedMarkerV1TagTo(MutableBlob& data)
{
    SeratoMarker::addEncodedMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Empty,
                                                    0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu,
                                                    Color{ 0 }, data);
}

// -- Instance Methods

void SeratoCueMarker::addMarkerV2TagTo(MutableBlob& data) const
{
    SeratoCueTagV2Struct header;

    memcpy(header.tag, "CUE", 4);
    count size = sizeof(SeratoCueTagV2Struct) + this->p_label.length() + 1 - sizeof(SeratoCueTagV2HeaderStruct);
    Platform::writeBigEndianUInteger32ValueAt(static_cast<uinteger32>(size), header.size);
    Platform::writeBigEndianUInteger16ValueAt(this->p_index, header.index);
    Platform::writeBigEndianUInteger32ValueAt(this->p_positionInMilliseconds, header.position);
    header.color[0] = 0;
    header.color[1] = this->p_color.red();
    header.color[2] = this->p_color.green();
    header.color[3] = this->p_color.blue();
    header.loop_enabled = 0;
    header.loop_locked = 0;

    data.appendMemoryWithSize(reinterpret_cast<const byte*>(&header), sizeof(SeratoCueTagV2Struct));
    data.appendWithStringTermination(this->p_label.asUTF8());
}

void SeratoCueMarker::addRawMarkerV1TagTo(MutableBlob& data) const
{
    SeratoMarker::addRawMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Cue,
                                                this->p_positionInMilliseconds,
                                                0xFFFFFFFF,
                                                0xFFFFFFFF,
                                                this->p_color,
                                                data);
}

void SeratoCueMarker::addEncodedMarkerV1TagTo(MutableBlob& data) const
{
    SeratoMarker::addEncodedMarkerV1TagWithFieldsTo(SeratoMarker::MarkerType::Cue,
                                                    this->p_positionInMilliseconds,
                                                    0xFFFFFFFF,
                                                    0xFFFFFFFF,
                                                    this->p_color,
                                                    data);
}
