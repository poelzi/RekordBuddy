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

#include <CommonCollection/Tracks/TrackPredicate.hpp>

using namespace NxA;
using namespace NxA::Common;

// -- Factory Methods

TrackPredicate TrackPredicate::fromBlob(const Blob& blob)
{
    return TrackPredicate{ blob };
}

TrackPredicate TrackPredicate::comparisonWith(Common::Property::TypeID uniqueID,
                                              TrackPredicate::ComparisonOperator comparisonOperator,
                                              String value)
{
    return TrackPredicate{ uniqueID, comparisonOperator, value };
}

TrackPredicate TrackPredicate::compoundWith(const TrackPredicate& leftSide,
                                            TrackPredicate::CompoundOperator compoundOperator,
                                            const TrackPredicate& rightSide)
{
    return TrackPredicate{ leftSide, compoundOperator, rightSide };
}

// -- Constructors & Destructors

TrackPredicate::TrackPredicate(const TrackPredicate& content,
                               count offset) : Blob{ static_cast<const Blob&>(content) }
{
    auto otherheader = content.commonHeader();
    count size = Platform::bigEndianUInteger32ValueAt(&otherheader->size);
    p_slice = std::make_tuple(offset, size);
}

TrackPredicate::TrackPredicate(Common::Property::TypeID tagID,
                               TrackPredicate::ComparisonOperator comparisonOperator,
                               const String& value)
{
    ComparisonDataHeader header;
    header.header.type = Type::Comparison;

    Platform::writeBigEndianUInteger32ValueAt(static_cast<uinteger32>(value.length() +
                                                                      1 +
                                                                      sizeof(ComparisonDataHeader)), &header.header.size);

    header.tagID = tagID;
    header.comparisonOperator = comparisonOperator;

    auto content = MutableBlob::withMemoryAndSize(reinterpret_cast<byte*>(&header), sizeof(header));
    content.appendWithStringTermination(value.asUTF8());

    this->Blob::operator=(content);
}

TrackPredicate::TrackPredicate(const TrackPredicate& leftSide,
                               TrackPredicate::CompoundOperator compoundOperator,
                               const TrackPredicate& rightSide)
{
    CompoundDataHeader header;
    header.header.type = Type::Compound;

    Platform::writeBigEndianUInteger32ValueAt(static_cast<uinteger32>(leftSide.size() +
                                                                      rightSide.size() +
                                                                      sizeof(CompoundDataHeader)),
                                              &header.header.size);

    header.compoundOperator = compoundOperator;

    auto content = MutableBlob::withMemoryAndSize(reinterpret_cast<byte*>(&header), sizeof(header));
    content.append(leftSide);
    content.append(rightSide);

    this->Blob::operator=(content);
}

// -- Operators

bool TrackPredicate::operator==(const TrackPredicate& other) const noexcept
{
    if (this->size() != other.size()) {
        return false;
    }

    for (count i = 0; i < this->size(); ++i) {
        if (*(this->data() + this->offset() + i) != *(other.data() + other.offset() + i)) {
            return false;
        }
    }

    return true;
}

// -- Instance Methods

Blob TrackPredicate::asBlob() const
{
    return *this;
}
