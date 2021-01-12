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

#include <CommonCollection/Markers/MarkerValidation.hpp>

#include <Base/Map.hpp>
#include <Base/Set.hpp>

using namespace NxA;
using namespace NxA::Common;

// -- Static Methods

Shared<Array<Common::MarkerOfSomeSort>> p_markersWithoutGridMarkers(const Array<Common::MarkerOfSomeSort>& markers)
{
    MutableArray<Common::MarkerOfSomeSort> markersWithoutGridMarkers;

    for (auto&& marker : markers) {
        auto maybeGridMarker = marker.maybeGet<NotNull<const Common::GridMarker*>>();
        if (maybeGridMarker.isValid()) {
            continue;
        }

        markersWithoutGridMarkers.append(marker);
    }

    return { Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(markersWithoutGridMarkers)) };
}

Shared<MutableArray<Common::MarkerOfSomeSort>> p_mutableArrayWithoutNegativeGridMarkersFrom(const Array<Common::MarkerOfSomeSort>& markers)
{
    auto markersWithoutNegativeGridMarkers = Shared<MutableArray<Common::MarkerOfSomeSort>>::with();

    for (auto&& marker : markers) {
        auto maybeGridMarker = marker.maybeGet<NotNull<const Common::GridMarker*>>();
        if (!maybeGridMarker.isValid()) {
            continue;
        }

        auto position = (*maybeGridMarker)->positionInSeconds();
        if (position.isNegative()) {
            continue;
        }

        markersWithoutNegativeGridMarkers->append(marker);
    }

    return markersWithoutNegativeGridMarkers;
}

// -- Instance Methods

NotNull<Common::MutableGridMarker*> MarkerValidation::p_newGridMarkerAsCopyOf(const Common::GridMarker& other)
{
    if (!this->p_newMarkers.isValid()) {
        this->p_newMarkers = MutableList<Common::MutableUtilityMarkerOfSomeSort>{ };
    }

    this->p_newMarkers->append(Common::MutableUtilityGridMarker{ other });

    return { &(this->p_newMarkers->lastObject().get<Common::MutableUtilityGridMarker>()) };
}

NotNull<Common::MutableCueMarker*> MarkerValidation::p_newCueMarkerAsCopyOf(const Common::CueMarker& other)
{
    if (!this->p_newMarkers.isValid()) {
        this->p_newMarkers = MutableList<Common::MutableUtilityMarkerOfSomeSort>{ };
    }

    this->p_newMarkers->append(Common::MutableUtilityCueMarker{ other });

    return { &(this->p_newMarkers->lastObject().get<Common::MutableUtilityCueMarker>()) };
}

NotNull<Common::MutableLoopMarker*> MarkerValidation::p_newLoopMarkerAsCopyOf(const Common::LoopMarker& other)
{
    if (!this->p_newMarkers.isValid()) {
        this->p_newMarkers = MutableList<Common::MutableUtilityMarkerOfSomeSort>{ };
    }

    this->p_newMarkers->append(Common::MutableUtilityLoopMarker{ other });

    return { &(this->p_newMarkers->lastObject().get<Common::MutableUtilityLoopMarker>()) };
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::p_maybeMarkersWithoutNegativeMarkers(const Array<Common::MarkerOfSomeSort>& markers)
{
    Optional<NotNull<const Common::GridMarker*>> maybeLastNegativeGridMarker;
    Optional<DecimalNumber> maybeFirstPositiveGridMarkerPosition;

    for (auto&& marker : markers) {
        auto maybeGridMarker = marker.maybeGet<NotNull<const Common::GridMarker*>>();
        if (!maybeGridMarker.isValid()) {
            continue;
        }

        auto position = (*maybeGridMarker)->positionInSeconds();
        if (!position.isNegative()) {
            if (!maybeFirstPositiveGridMarkerPosition.isValid() || (position < *maybeFirstPositiveGridMarkerPosition)) {
                maybeFirstPositiveGridMarkerPosition = position;
            }
        }
        else if (!maybeLastNegativeGridMarker.isValid() || (position > (*maybeLastNegativeGridMarker)->positionInSeconds())) {
            maybeLastNegativeGridMarker = maybeGridMarker;
        }
    }

    if (!maybeLastNegativeGridMarker.isValid()) {
        // -- We didn't find any negative markers so we are done.
        return nothing;
    }

    auto beatNumber = (*maybeLastNegativeGridMarker)->beatNumber();
    count fixCount = 0;

    auto start = (*maybeLastNegativeGridMarker)->positionInSeconds();
    auto beatLength = DecimalNumber::withInteger(60) / (*maybeLastNegativeGridMarker)->beatsPerMinute();

    do {
        if (++fixCount > 123) {
            // -- If the grid marker seems too far in the negative, then we assume it's garbage and ignore it.
            return nothing;
        }

        start += beatLength;
        beatNumber = Common::GridMarker::beatNumberAfter(beatNumber);
    }
    while (start.isNegative());

    if (maybeFirstPositiveGridMarkerPosition.isValid() && (start >= *maybeFirstPositiveGridMarkerPosition)) {
        // -- The marker we calculated to replace the last negative grid marker is actually after an existing positive marker so we don't need to add it in the end.
        return { p_mutableArrayWithoutNegativeGridMarkersFrom(markers) };
    }

    auto newMarker = this->p_newGridMarkerAsCopyOf(**maybeLastNegativeGridMarker);
    newMarker->setPositionInSeconds(start);
    newMarker->setBeatNumber(beatNumber);

    auto mutableMarkerArray = p_mutableArrayWithoutNegativeGridMarkersFrom(markers);
    mutableMarkerArray->append(newMarker);

    return { Shared<MutableArray<Common::MarkerOfSomeSort>>::with(std::move(*mutableMarkerArray)) };
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::p_maybeMarkersWithHotLoopsReAssigned(const Array<Common::MarkerOfSomeSort>& markers)
{
    Optional<count> maybeFirstHotLoopNumber;

    for (auto&& marker : markers) {
        auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
        if (maybeLoopMarker.isValid()) {
            auto maybeHotCueNumber = (*maybeLoopMarker)->maybeHotCueNumber();
            if (maybeHotCueNumber.isValid() && (!maybeFirstHotLoopNumber.isValid() || *maybeHotCueNumber < *maybeFirstHotLoopNumber)) {
                maybeFirstHotLoopNumber = maybeHotCueNumber;
            }
        }
    }

    if (!maybeFirstHotLoopNumber.isValid()) {
        return nothing;
    }

    MutableArray<Common::MarkerOfSomeSort> correctedMarkers;
    count maximumHotLoopNumber = *this->p_maybeMaximumHotLoopNumber;

    for (auto&& marker : markers) {
        auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
        if (maybeLoopMarker.isValid()) {
            auto maybeHotCueNumber = (*maybeLoopMarker)->maybeHotCueNumber();
            if (maybeHotCueNumber.isValid()) {
                count newHotCueNumber = *maybeHotCueNumber - *maybeFirstHotLoopNumber;
                if (newHotCueNumber > maximumHotLoopNumber) {
                    continue;
                }

                auto newMarker = this->p_newLoopMarkerAsCopyOf(**maybeLoopMarker);
                newMarker->setHotCueNumber(newHotCueNumber);
                correctedMarkers.append(std::move(newMarker));

                continue;
            }
        }

        correctedMarkers.append(marker);
    }

    return { Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(correctedMarkers)) };
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::p_maybeMarkersWithoutGridMarkersIfInvalid(const Array<Common::MarkerOfSomeSort>& markers,
                                                                                                              boolean onlyGridMarkersOnFirstDownBeat,
                                                                                                              boolean onlyOneBeatsPerMinuteForAllGridMarkers,
                                                                                                              MutableArray<String>* reasons)
{
    Optional<DecimalNumber> maybeTrackUniqueBeatsPerMinute;

    for (auto&& marker : markers) {
        auto maybeGridMarker = marker.maybeGet<NotNull<const Common::GridMarker *>>();
        if (maybeGridMarker.isValid()) {
            boolean foundInvalid = false;

            if (onlyOneBeatsPerMinuteForAllGridMarkers) {
                if (!maybeTrackUniqueBeatsPerMinute.isValid()) {
                    maybeTrackUniqueBeatsPerMinute = maybeGridMarker->get()->beatsPerMinute();
                }
                else if (maybeGridMarker->get()->beatsPerMinute() != *maybeTrackUniqueBeatsPerMinute) {
                    foundInvalid = true;

                    if (reasons) {
                        reasons->append("Grid markers should all have the same BPM as the track BPM."_String);
                    }
                }
            }
            else if (onlyGridMarkersOnFirstDownBeat && (maybeGridMarker->get()->beatNumber() != GridMarker::BeatNumber::FirstDownBeat)) {
                foundInvalid = true;

                if (reasons) {
                    reasons->append("Grid markers need to all be on the first down beat"_String);
                }
            }

            if (foundInvalid) {
                return p_markersWithoutGridMarkers(markers);
            }
        }
    }

    return nothing;
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::p_maybeMarkersWithoutIllegalHotCueNumbers(const Array<Common::MarkerOfSomeSort>& markers)
{
    boolean foundAHotCueNumberTooHigh = false;
    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid()) {
            auto maybeHotCueNumber = (*maybeCueMarker)->maybeHotCueNumber();
            if (maybeHotCueNumber.isValid() && (*maybeHotCueNumber >= this->p_maximumNumberOfHotCues)) {
                foundAHotCueNumberTooHigh = true;
                break;
            }
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid()) {
                auto maybeHotCueNumber = (*maybeLoopMarker)->maybeHotCueNumber();
                if (maybeHotCueNumber.isValid() && (*maybeHotCueNumber >= this->p_maximumNumberOfHotCues)) {
                    foundAHotCueNumberTooHigh = true;
                    break;
                }
            }
        }
    }

    if (!foundAHotCueNumberTooHigh) {
        return nothing;
    }

    MutableArray<Common::MarkerOfSomeSort> correctedMarkers;

    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid()) {
            auto maybeHotCueNumber = (*maybeCueMarker)->maybeHotCueNumber();
            if (maybeHotCueNumber.isValid() && (*maybeHotCueNumber >= this->p_maximumNumberOfHotCues)) {
                continue;
            }
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid()) {
                auto maybeHotCueNumber = (*maybeLoopMarker)->maybeHotCueNumber();
                if (maybeHotCueNumber.isValid() && (*maybeHotCueNumber >= this->p_maximumNumberOfHotCues)) {
                    continue;
                }
            }
        }

        correctedMarkers.append(marker);
    }

    return { Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(correctedMarkers)) };
}

Shared<Array<Common::MarkerOfSomeSort>> MarkerValidation::p_markersWithNoMoreMemoryCuesThan(const Array<Common::MarkerOfSomeSort>& markers,
                                                                                            count maximumNumberOfMemoryCues)
{
    count numberOfMemoryCuesFound = 0;
    MutableArray<Common::MarkerOfSomeSort> correctedMarkers;

    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid() && !(*maybeCueMarker)->maybeHotCueNumber().isValid() && (++numberOfMemoryCuesFound > maximumNumberOfMemoryCues)) {
            continue;
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid() && !(*maybeLoopMarker)->maybeHotCueNumber().isValid() && (++numberOfMemoryCuesFound > maximumNumberOfMemoryCues)) {
                continue;
            }
        }

        correctedMarkers.append(marker);
    }

    return Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(correctedMarkers));
}

count MarkerValidation::p_numberOfMemoryCuesIn(const Array<Common::MarkerOfSomeSort>& markers)
{
    count numberOfMemoryCues = 0;

    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid() && !(*maybeCueMarker)->maybeHotCueNumber().isValid()) {
            ++numberOfMemoryCues;
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid() && !(*maybeLoopMarker)->maybeHotCueNumber().isValid()) {
                ++numberOfMemoryCues;
            }
        }
    }

    return numberOfMemoryCues;
}

boolean MarkerValidation::p_foundAHotCueIn(const Array<Common::MarkerOfSomeSort>& markers)
{
    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid() && (*maybeCueMarker)->maybeHotCueNumber().isValid()) {
            return true;
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid() && (*maybeLoopMarker)->maybeHotCueNumber().isValid()) {
                return true;
            }
        }
    }

    return false;
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues(const Array<Common::MarkerOfSomeSort>& markers,
                                                                                                                    count& numberOfMemoryCuesAlreadyThere,
                                                                                                                    count maximumNumberOfMemoryCues,
                                                                                                                    boolean& foundAHotCue)
{
    auto memoryCueIndex = numberOfMemoryCuesAlreadyThere;
    boolean foundAHotCueToDuplicate = false;
    foundAHotCue = false;

    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid() && (*maybeCueMarker)->maybeHotCueNumber().isValid()) {
            foundAHotCue = true;

            if (++memoryCueIndex <= maximumNumberOfMemoryCues) {
                foundAHotCueToDuplicate = true;
                break;
            }
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid() && (*maybeLoopMarker)->maybeHotCueNumber().isValid()) {
                foundAHotCue = true;

                if (++memoryCueIndex <= maximumNumberOfMemoryCues) {
                    foundAHotCueToDuplicate = true;
                    break;
                }
            }
        }
    }

    if (!foundAHotCueToDuplicate) {
        return nothing;
    }

    MutableArray<Common::MarkerOfSomeSort> correctedMarkers;

    for (auto&& marker : markers) {
        correctedMarkers.append(marker);

        if (numberOfMemoryCuesAlreadyThere == maximumNumberOfMemoryCues) {
            continue;
        }

        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid() && (*maybeCueMarker)->maybeHotCueNumber().isValid()) {
            auto newMarker = this->p_newCueMarkerAsCopyOf(**maybeCueMarker);
            newMarker->setHotCueNumber(nothing);
            correctedMarkers.append(std::move(newMarker));

            ++numberOfMemoryCuesAlreadyThere;
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid() && (*maybeLoopMarker)->maybeHotCueNumber().isValid()) {
                auto newMarker = this->p_newLoopMarkerAsCopyOf(**maybeLoopMarker);
                newMarker->setHotCueNumber(nothing);
                correctedMarkers.append(std::move(newMarker));

                ++numberOfMemoryCuesAlreadyThere;
            }
        }
    }

    return { Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(correctedMarkers)) };
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::p_maybeMarkersWithMemoryCuesConvertedToHotCues(const Array<Common::MarkerOfSomeSort>& markers,
                                                                                                                   boolean& foundAHotCue)
{
    integer64 maximumHotCueNumberFound = -1;

    foundAHotCue = false;
    boolean foundAMemoryCue = false;

    for (auto&& marker : markers) {
        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid()) {
            auto maybeHotCueNumber = (*maybeCueMarker)->maybeHotCueNumber();
            if (maybeHotCueNumber.isValid()) {
                foundAHotCue = true;

                if (*maybeNarrowCast<integer64>(*maybeHotCueNumber) > maximumHotCueNumberFound) {
                    maximumHotCueNumberFound = *maybeHotCueNumber;
                }
            }
            else {
                foundAMemoryCue = true;
            }
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid()) {
                auto maybeHotCueNumber = (*maybeLoopMarker)->maybeHotCueNumber();
                if (maybeHotCueNumber.isValid()) {
                    foundAHotCue = true;

                    if (*maybeNarrowCast<integer64>(*maybeHotCueNumber) > maximumHotCueNumberFound) {
                        maximumHotCueNumberFound = *maybeHotCueNumber;
                    }
                }
                else {
                    foundAMemoryCue = true;
                }
            }
        }
    }

    auto nextHotCueNumber = maximumHotCueNumberFound + 1;
    auto maximumNumberOfHotCues = *maybeNarrowCast<integer64>(this->p_maximumNumberOfHotCues);
    if (!foundAMemoryCue || (nextHotCueNumber >= maximumNumberOfHotCues)) {
        return nothing;
    }

    MutableArray<Common::MarkerOfSomeSort> correctedMarkers;

    for (auto&& marker : markers) {
        correctedMarkers.append(marker);

        if (nextHotCueNumber >= maximumNumberOfHotCues) {
            continue;
        }

        auto maybeCueMarker = marker.maybeGet<NotNull<const Common::CueMarker*>>();
        if (maybeCueMarker.isValid() && !(*maybeCueMarker)->maybeHotCueNumber().isValid()) {
            auto newMarker = this->p_newCueMarkerAsCopyOf(**maybeCueMarker);
            newMarker->setHotCueNumber(nextHotCueNumber++);
            correctedMarkers.append(std::move(newMarker));
        }
        else {
            auto maybeLoopMarker = marker.maybeGet<NotNull<const Common::LoopMarker*>>();
            if (maybeLoopMarker.isValid() && !(*maybeLoopMarker)->maybeHotCueNumber().isValid()) {
                auto newMarker = this->p_newLoopMarkerAsCopyOf(**maybeLoopMarker);
                newMarker->setHotCueNumber(nextHotCueNumber++);
                correctedMarkers.append(std::move(newMarker));
            }
        }
    }

    return { Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(correctedMarkers)) };
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::maybeCorrectedMarkersWithWhenExportedTo(const Array<Common::MarkerOfSomeSort>& markers,
                                                                                                            Common::Collection::Type collectionType,
                                                                                                            MutableArray<String>* reasons)
{
    if (!markers.length()) {
        // -- If we don't have any markers so we have nothing to correct.
        return nothing;
    }

    boolean onlyGridMarkersOnFirstDownBeat = false;
    boolean onlyOneBeatsPerMinuteForAllGridMarkers = false;
    boolean noGridMarkersWithNegativePosition = false;
    count maximumNumberOfMemoryCues = 8;

    switch (collectionType) {
        case Common::Collection::Type::rekordbox: {
            // -- rekordbox does not like grid markers with negative positions.
            noGridMarkersWithNegativePosition = true;
            break;
        }
        case Common::Collection::Type::Serato: {
            // -- Serato only supports grid markers that are on the first downbeat so we remove the beat grid.
            onlyGridMarkersOnFirstDownBeat = true;

            // -- Serato separates hot loops from hot cues so these can be output separately from hotcues.
            this->p_maybeMaximumHotLoopNumber = 8;

            // -- Serato doesn't support memory cues.
            maximumNumberOfMemoryCues = 0;
            break;
        }
        case Common::Collection::Type::Traktor: {
            // -- Traktor does not like grid markers with negative positions.
            noGridMarkersWithNegativePosition = true;

            // -- Traktor only supports grid markers that are all using the same beats per minute value.
            onlyOneBeatsPerMinuteForAllGridMarkers = true;
            break;
        }
        default: {
            // -- By default we don't output any markers so that we don't get it wrong.
            return nothing;
        }
    }

    Optional<Shared<Array<Common::MarkerOfSomeSort>>> maybeCorrectedMarkers;

    if (noGridMarkersWithNegativePosition) {
        maybeCorrectedMarkers = this->p_maybeMarkersWithoutNegativeMarkers(markers);
        if ((reasons != nullptr) && maybeCorrectedMarkers.isValid()) {
            reasons->append("Grid markers cannot be at a negative position."_String);
        }
    }

    if (this->p_maybeMaximumHotLoopNumber.isValid()) {
        auto result = this->p_maybeMarkersWithHotLoopsReAssigned(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers);
        if (result.isValid()) {
            maybeCorrectedMarkers = result;
        }
    }

    boolean gridMarkersHaveBeenRemoved = false;
    if (onlyGridMarkersOnFirstDownBeat || onlyOneBeatsPerMinuteForAllGridMarkers) {
        auto result = this->p_maybeMarkersWithoutGridMarkersIfInvalid(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers,
                                                                      onlyGridMarkersOnFirstDownBeat,
                                                                      onlyOneBeatsPerMinuteForAllGridMarkers,
                                                                      reasons);
        if (result.isValid()) {
            gridMarkersHaveBeenRemoved = true;
            maybeCorrectedMarkers = result;
        }
    }

    count numberOfMemoryCues = this->p_numberOfMemoryCuesIn(markers);

    boolean foundAHotCue = false;
    if (this->p_correctingFlags.has(CorrectingFlag::DuplicateHotCuesAlsoAsMemoryCues) && (maximumNumberOfMemoryCues > 0)) {
        auto result = this->p_maybeMarkersWithHotCuesDuplicatedAsMemoryCues(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers,
                                                                            numberOfMemoryCues, maximumNumberOfMemoryCues, foundAHotCue);
        if (result.isValid()) {
            maybeCorrectedMarkers = result;
            result = nothing;
        }
    }
    else if (this->p_correctingFlags.has(CorrectingFlag::ConvertMemoryCuesToHotCues)) {
        auto result = this->p_maybeMarkersWithMemoryCuesConvertedToHotCues(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers, foundAHotCue);
        if (result.isValid()) {
            maybeCorrectedMarkers = result;
            result = nothing;
        }
    }
    else {
        foundAHotCue = MarkerValidation::p_foundAHotCueIn(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers);
    }

    if (numberOfMemoryCues > maximumNumberOfMemoryCues) {
        maybeCorrectedMarkers = this->p_markersWithNoMoreMemoryCuesThan(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers,
                                                                        maximumNumberOfMemoryCues);
        if (reasons != nullptr) {
            if (maximumNumberOfMemoryCues) {
                reasons->append(String::stringWithFormat("Cannot have more than %llu memory cues (found %llu).", maximumNumberOfMemoryCues, numberOfMemoryCues));
            }
            else {
                reasons->append(String::stringWithFormat("Does not support memory cues (found %llu).", numberOfMemoryCues));
            }
        }
    }

    if (foundAHotCue) {
        auto result = this->p_maybeMarkersWithoutIllegalHotCueNumbers(maybeCorrectedMarkers.isValid() ? *maybeCorrectedMarkers->asRawPointer() : markers);
        if (result.isValid()) {
            maybeCorrectedMarkers = result;
            result = nothing;

            if (reasons != nullptr) {
                reasons->append(String::stringWithFormat("Contains illegal hotcue numbers (maximum is %llu).", this->p_maximumNumberOfHotCues));
            }
        }
    }

    return maybeCorrectedMarkers;
}

Optional<Shared<Array<Common::MarkerOfSomeSort>>> MarkerValidation::maybeMarkersWithHotCuesAndGridMarkersMerged(const Array<Common::MarkerOfSomeSort>& markers)
{
    MutableMap<count, count> hotCueNumberPerGridMarkerIndex;
    MutableSet<count> usedHotCueHotLoopMarkerIndices;
    count firstMarkerIndex = 0;

    for (auto&& firstMarker : markers) {
        auto maybeGridMarker = firstMarker.maybeGet<NotNull<const Common::GridMarker *>>();
        if (maybeGridMarker.isValid()) {
            Optional<count> maybeHotCueNumber;
            count secondMarkerIndexPlusOne = 0;

            for (auto&& secondMarker : markers) {
                auto secondMarkerIndex = secondMarkerIndexPlusOne++;

                if ((secondMarker == firstMarker) || usedHotCueHotLoopMarkerIndices.contains(secondMarkerIndex)) {
                    continue;
                }

                auto maybeCueMarker = secondMarker.maybeGet<NotNull<const Common::CueMarker*>>();
                if (maybeCueMarker.isValid()) {
                    maybeHotCueNumber = (*maybeCueMarker)->maybeHotCueNumber();
                    if (!maybeHotCueNumber.isValid() ||
                        ((*maybeCueMarker)->positionInSeconds() != (*maybeGridMarker)->positionInSeconds())) {
                        continue;
                    }
                }
                else {
                    auto maybeLoopMarker = secondMarker.maybeGet<NotNull<const Common::LoopMarker*>>();
                    if (maybeLoopMarker.isValid()) {
                        maybeHotCueNumber = (*maybeLoopMarker)->maybeHotCueNumber();
                        if (!maybeHotCueNumber.isValid() ||
                            ((*maybeLoopMarker)->positionInSeconds() != (*maybeGridMarker)->positionInSeconds())) {
                            continue;
                        }
                    }
                    else {
                        continue;
                    }
                }

                usedHotCueHotLoopMarkerIndices.add(secondMarkerIndex);
                hotCueNumberPerGridMarkerIndex.setValueForKey(*maybeHotCueNumber, firstMarkerIndex);
                break;
            }
        }

        ++firstMarkerIndex;
    }

    if (usedHotCueHotLoopMarkerIndices.length() == 0) {
        return nothing;
    }

    MutableArray<Common::MarkerOfSomeSort> correctedMarkers;

    count markerIndexPlusOne = 0;
    for (auto&& marker : markers) {
        auto markerIndex = markerIndexPlusOne++;

        auto maybeGridMarker = marker.maybeGet<NotNull<const Common::GridMarker *>>();
        if (maybeGridMarker.isValid()) {
            auto maybeHotCueNumber = hotCueNumberPerGridMarkerIndex.maybeValueForKey(markerIndex);
            if (maybeHotCueNumber.isValid()) {
                auto newMarker = this->p_newGridMarkerAsCopyOf(**maybeGridMarker);
                newMarker->setHotCueNumber(*maybeHotCueNumber);

                correctedMarkers.append(newMarker);

                continue;
            }
        }
        else if (usedHotCueHotLoopMarkerIndices.contains(markerIndex)) {
            continue;
        }

        correctedMarkers.append(marker);
    }

    return { Shared<Array<Common::MarkerOfSomeSort>>::with(std::move(correctedMarkers)) };
}

