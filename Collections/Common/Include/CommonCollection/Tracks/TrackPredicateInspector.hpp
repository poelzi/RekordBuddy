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

#include <CommonCollection/Tracks/TrackPredicate.hpp>

#include <Base/Blob.hpp>
#include <Base/Base.hpp>

namespace NxA { namespace Common {

template <typename Inspector>
    typename Inspector::Result inspectTrackPredicateUsing(const TrackPredicate&, Inspector);

template <typename Inspector>
    decltype(auto) inspectCompoundTrackPredicateUsing(const TrackPredicate& tag, Inspector inspector)
    {
        auto compoundHeader = tag.compoundHeader();
        auto loffset = tag.leftSideOffset();
        auto roffset = tag.rightSideOffset();
        TrackPredicate ltag{ tag, loffset };
        TrackPredicate rtag{ tag, roffset };

        return inspector.fromCompoundPredicate(tag,
                                               inspectTrackPredicateUsing(ltag, inspector),
                                               compoundHeader->compoundOperator,
                                               inspectTrackPredicateUsing(rtag, inspector));
    }

template <typename Inspector>
    decltype(auto) inspectComparisonTrackPredicateUsing(const TrackPredicate& tag, Inspector inspector)
    {
        auto comparisonHeader = tag.comparisonHeader();
        return inspector.fromComparisonPredicate(tag,
                                                 comparisonHeader->tagID,
                                                 comparisonHeader->comparisonOperator,
                                                 String{comparisonHeader->value});
    }

template <typename Inspector>
    typename Inspector::Result inspectTrackPredicateUsing(const TrackPredicate& tag, Inspector inspector)
    {
        auto common = tag.commonHeader();
        switch (common->type) {
            case TrackPredicate::Type::Compound: {
                return inspectCompoundTrackPredicateUsing(tag, inspector);
            }
            case TrackPredicate::Type::Comparison: {
                return inspectComparisonTrackPredicateUsing(tag, inspector);
            }
            default: {
                NXA_ALOG_WITH_FORMAT("Illegal predicate type 0x%02hhx.", common->type);
            }
        }
    }

} }
