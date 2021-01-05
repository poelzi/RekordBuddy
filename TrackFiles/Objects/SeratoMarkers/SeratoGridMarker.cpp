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

#include <math.h>

using namespace NxA;

// -- Factory Methods

SeratoGridMarker SeratoGridMarker::markerWithPositionAndBeatsPerMinute(const DecimalNumber& positionInSeconds,
                                                                       const DecimalNumber& beatsPerMinute)
{
    SeratoGridMarker newMarker;

    newMarker.p_positionInSeconds = positionInSeconds;
    newMarker.p_beatsPerMinute = beatsPerMinute;

    return newMarker;
}

// -- Class Methods

void SeratoGridMarker::addMarkersWithMemoryAtTo(const byte* id3TagStart,
                                                count size,
                                                MutableArray<SeratoMarker::OfSomeSort>& markers)
{
    if (size < 4) {
        return;
    }

    auto numberOfMarkers = Platform::bigEndianUInteger32ValueAt(id3TagStart);
    if (!numberOfMarkers) {
        return;
    }
    else if (numberOfMarkers > 256) {
        numberOfMarkers = 256;
    }

    DecimalNumber nextPosition;
    static auto sixty = DecimalNumber::withInteger(60);
    static auto one = DecimalNumber::withInteger(1);

    auto gridMarker = reinterpret_cast<const SeratoGridMarker::GridMarkerStruct*>(id3TagStart + 4);
    size -= 4;

    for (count markerIndex = 0; markerIndex < numberOfMarkers; ++markerIndex) {
        if (size < sizeof(SeratoGridMarker::GridMarkerStruct)) {
            return;
        }

        size -= sizeof(SeratoGridMarker::GridMarkerStruct);

        DecimalNumber position;
        if (markerIndex) {
            position = nextPosition;
        }
        else {
            position = DecimalNumber(Platform::bigEndianFloatValueAt(gridMarker->positionInSeconds),
                                     DecimalNumber::UsingRoundingPolicy::CloserToZero);
        }

        DecimalNumber bpm;

        const SeratoGridMarker::GridMarkerStruct* nextGridMarker = gridMarker + 1;
        if ((markerIndex + 1) < numberOfMarkers) {
            if (size < sizeof(SeratoGridMarker::GridMarkerStruct)) {
                return;
            }

            nextPosition = DecimalNumber(Platform::bigEndianFloatValueAt(nextGridMarker->positionInSeconds));

            if (nextPosition == position) {
                gridMarker = nextGridMarker;
                continue;
            }

            DecimalNumber numberOfBeats(Platform::bigEndianUInteger32ValueAt(gridMarker->beatsPerMinute));
            bpm = (numberOfBeats * sixty) / (nextPosition - position);
        }
        else {
            bpm = DecimalNumber(Platform::bigEndianFloatValueAt(gridMarker->beatsPerMinute));
        }

        // -- This is a workaround for some invalid grid marker positions we are
        // -- getting in some Serato tracks. Not sure if we are reading those wrong
        // -- or if those are corrupted but this will do for now until we figure it out.
        if (bpm > one) {
            markers.append(SeratoGridMarker::markerWithPositionAndBeatsPerMinute(position,
                                                                                 bpm.withTwoPrecisionDigits(DecimalNumber::UsingRoundingPolicy::CloserToZero)));
        }

        gridMarker = nextGridMarker;
    }
}

Blob SeratoGridMarker::gridMarkerDataFrom(const MutableArray<SeratoMarker::OfSomeSort>& markers)
{
    MutableBlob result;

    MutableArray<NotNull<const SeratoGridMarker*>> gridMarkers;
    for (auto&& marker : markers) {
        if (!marker.isType<SeratoGridMarker>()) {
            continue;
        }

        gridMarkers.emplaceAppend(&marker.get<SeratoGridMarker>());
    }

    count numberOfGridMarkers = gridMarkers.length();

    uinteger32 numberOfMarkersBuffer;
    Platform::writeBigEndianUInteger32ValueAt(static_cast<uinteger32>(numberOfGridMarkers), reinterpret_cast<byte*>(&numberOfMarkersBuffer));
    auto numberOfMarkersData = Blob::withMemoryAndSize(reinterpret_cast<byte*>(&numberOfMarkersBuffer), sizeof(numberOfMarkersBuffer));
    result.append(numberOfMarkersData);

    static auto sixty = DecimalNumber::withInteger(60);

    for (count index = 0; index < numberOfGridMarkers; ++index) {
        auto gridMarker = gridMarkers[index];
        SeratoGridMarker::GridMarkerStruct markerData;

        auto position = gridMarker->positionInSeconds();
        Platform::writeBigEndianFloatValueAt(position.asDouble(), markerData.positionInSeconds);

        count nextIndex = index + 1;
        if (nextIndex >= numberOfGridMarkers) {
            Platform::writeBigEndianFloatValueAt(gridMarker->beatsPerMinute().asDouble(), markerData.beatsPerMinute);
        }
        else {
            auto nextGridMarker = gridMarkers[nextIndex];

            auto nextPosition = nextGridMarker->positionInSeconds();
            DecimalNumber numberOfBeats = (gridMarker->beatsPerMinute() * (nextPosition - position)) / sixty;

            count actualNumberOfBeats = SeratoGridMarker::p_actualNumberOfBeatsIfSupported(numberOfBeats);
            if (!actualNumberOfBeats) {
                return { };
            }

            Platform::writeBigEndianUInteger32ValueAt(static_cast<uinteger32>(actualNumberOfBeats),
                                                      markerData.beatsPerMinute);
        }

        result.appendMemoryWithSize(reinterpret_cast<const byte*>(&markerData),
                                    sizeof(SeratoGridMarker::GridMarkerStruct));
    }

    // -- This marks the end of tags.
    result.append('\0');

    return { std::move(result) };
}

boolean SeratoGridMarker::gridMarkersAreValid(const Array<SeratoGridMarker>& markers)
{
    // -- Serato doesn't support very flexible grid markers, the ones we end up writing
    // -- might not be exactly the ones we wanted to write so we test them
    // -- before setting them and if at least one is invalid, we write none.
    count numberOfMarkers = markers.length();
    count lastMarkerIndex = numberOfMarkers - 1;

    static auto sixty = DecimalNumber::withInteger(60);

    for (count index = 0; index < numberOfMarkers; ++index) {
        auto& marker = markers[index];

        if (index != lastMarkerIndex) {
            auto& nextMarker = markers[index + 1];

            auto markerPosition = marker.positionInSeconds();
            auto nextMarkerPosition = nextMarker.positionInSeconds();
            if (nextMarkerPosition == markerPosition) {
                return false;
            }

            DecimalNumber numberOfBeats = (marker.beatsPerMinute() * (nextMarkerPosition - markerPosition)) / sixty;

            count actualNumberOfBeats = SeratoGridMarker::p_actualNumberOfBeatsIfSupported(numberOfBeats);
            if (!actualNumberOfBeats) {
                return false;
            }
        }
    }

    return true;
}

// -- Instance Methods

void SeratoGridMarker::addMarkerV2TagTo(MutableBlob& data) const
{
    NXA_ALOG("Invalid call on a grid marker.");
}

void SeratoGridMarker::addRawMarkerV1TagTo(MutableBlob& data) const
{
    NXA_ALOG("Invalid call on a grid marker.");
}

void SeratoGridMarker::addEncodedMarkerV1TagTo(MutableBlob& data) const
{
    NXA_ALOG("Invalid call on a grid marker.");
}
