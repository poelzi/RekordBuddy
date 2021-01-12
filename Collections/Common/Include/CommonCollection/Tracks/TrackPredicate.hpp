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

#include <CommonCollection/Tracks/Property.hpp>

#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <Base/String.hpp>
#include <Base/Blob.hpp>
#include <Base/Types.hpp>

namespace NxA { namespace Common {

#if defined(NXA_BUILD_FOR_TESTING)
// -- Forward Declarations so unit test classes can be friends.
class TrackPredicateTests_AComparisonPredicate_comparisonWith_ReturnsCorrectPredicate_Test;
class TrackPredicateTests_ACompoundPredicate_compoundWith_ReturnsCorrectPredicate_Test;
#endif

class TrackPredicate final : private Blob
{
    // -- Friends
    template <typename Inspector>
        friend typename Inspector::Result inspectTrackPredicateUsing(const TrackPredicate&, Inspector);

    template <typename Inspector>
        friend decltype(auto) inspectCompoundTrackPredicateUsing(const TrackPredicate&, Inspector);

    template <typename Inspector>
        friend decltype(auto) inspectComparisonTrackPredicateUsing(const TrackPredicate&, Inspector);

#if defined(NXA_BUILD_FOR_TESTING)
    friend class TrackPredicateTests_AComparisonPredicate_comparisonWith_ReturnsCorrectPredicate_Test;
    friend class TrackPredicateTests_ACompoundPredicate_compoundWith_ReturnsCorrectPredicate_Test;
#endif

public:
    // -- Constants
    enum class ComparisonOperator : byte {
        // -- These are stored in user data and should not be re-ordered or modified.
        // -- New operators can be added to the list though.
        Equal,
        NotEqual,
        LessThan,
        LessThanOrEqual,
        GreaterThan,
        GreaterThanOrEqual,
        StartsWith,
        EndsWith,
        Contains,
        DoesNotContains,

        // -- TODO: We need to add options to the operators like
        // -- Case Insensitive or Diacritic Insensitive for strings for example.
    };

    enum class CompoundOperator : byte {
        // -- These are stored in user data and should not be re-ordered or modified.
        // -- New operators can be added to the list though.
        And,
        Or,
    };

    // -- We store these as a character so we need to make sure the casts are safe.
    static_assert(sizeof(ComparisonOperator) == sizeof(character), "TagComparisonPredicate::Operator should be the size of a character.");
    static_assert(sizeof(CompoundOperator) == sizeof(character), "TagCompoundPredicate::Operator should be the size of a character.");

protected:
    // -- Protected Constants
    enum class Type : byte {
        // -- These are stored in user data and should not be re-ordered or modified.
        // -- New types can be added to the list though.
        Comparison,
        Compound,
        Legacy = 98, // -- In case we load a legacy plist format, this lets us detect it and fail with a better error
    };

    // -- Protected Structures
    struct CommonDataHeader {
        // -- These are stored in user data and should not be re-ordered or modified.
        Type type;

        // -- Size is stored as a uinteger32 but we don't need the compiler to align it so we use bytes in the struct.
        byte size;
        byte restOfsize[3];
    };

    struct ComparisonDataHeader {
        CommonDataHeader header;

        // -- These are stored in user data and should not be re-ordered or modified.
        Common::Property::TypeID tagID;
        TrackPredicate::ComparisonOperator comparisonOperator;
        character value[0];
    };

    struct CompoundDataHeader {
        CommonDataHeader header;
        // -- These are stored in user data and should not be re-ordered or modified.
        TrackPredicate::CompoundOperator compoundOperator;
        character leftPredicate[0];
    };

    // -- We store these things as binary data so we need to make sure the casts are safe.
    static_assert(sizeof(Type) == sizeof(character), "TrackPredicate::Type should be the size of a character.");
    static_assert(sizeof(CommonDataHeader) == 5, "TrackPredicate::CommonDataHeader size is incorrect.");
    static_assert(sizeof(ComparisonDataHeader) == 7, "TrackPredicate::ComparisonDataHeader size is incorrect.");
    static_assert(sizeof(CompoundDataHeader) == 6, "TrackPredicate::CompoundDataHeader size is incorrect.");

private:
    // -- Private Instance Variables
    Optional<std::tuple<count, count>> p_slice;
    
    // -- Private Constructors & Destructors
    TrackPredicate() = default;
    explicit TrackPredicate(const Blob& content) : Blob{ content }, p_slice{nothing} { }
    explicit TrackPredicate(Blob&& content) : Blob{ std::move(content) }, p_slice{nothing} { }
    explicit TrackPredicate(const TrackPredicate&, count offset);
    explicit TrackPredicate(Common::Property::TypeID,
                            TrackPredicate::ComparisonOperator,
                            const String&);
    explicit TrackPredicate(const TrackPredicate&,
                            TrackPredicate::CompoundOperator,
                            const TrackPredicate&);

    // -- Private Instance Methods
    count offset() const
    {
        if (!this->p_slice.isValid()) {
            return 0;
        }

        return std::get<0>(*this->p_slice);
    }

    count size() const
    {
        if (!this->p_slice.isValid()) {
            return this->Blob::size();
        }

        return std::get<1>(*this->p_slice);
    }

    inline count leftSideOffset() const
    {
        return this->offset() + offsetof(CompoundDataHeader, leftPredicate);
    }

    inline NotNull<const CommonDataHeader*> leftSideOfCompound() const
    {
        auto header = this->compoundHeader();
        return reinterpret_cast<const CommonDataHeader*>(header->leftPredicate);
    }

    inline count rightSideOffset() const
    {
        auto leftSideDynamic = reinterpret_cast<const CommonDataHeader*>(&this->compoundHeader()->leftPredicate);
        return this->leftSideOffset() + Platform::bigEndianUInteger32ValueAt(&leftSideDynamic->size);
    }

    inline NotNull<const CommonDataHeader*> rightSideOfCompound() const
    {
        auto header = this->compoundHeader();
        auto leftSide = reinterpret_cast<const CommonDataHeader*>(&header->leftPredicate);
        return reinterpret_cast<const CommonDataHeader*>(header->leftPredicate + Platform::bigEndianUInteger32ValueAt(&leftSide->size));
    }

    inline NotNull<const CommonDataHeader*> commonHeader() const
    {
        return reinterpret_cast<const CommonDataHeader*>(data().get() + offset());
    }

    inline NotNull<const ComparisonDataHeader*> comparisonHeader() const
    {
        auto header = this->commonHeader();
        NXA_ASSERT_EQ(header->type, Type::Comparison);
        return reinterpret_cast<const ComparisonDataHeader*>(header.get());
    }

    inline NotNull<const CompoundDataHeader*> compoundHeader() const
    {
        auto header = this->commonHeader();
        NXA_ASSERT_EQ(header->type, Type::Compound);
        return reinterpret_cast<const CompoundDataHeader*>(header.get());
    }

public:
    // -- Factory Methods
    static TrackPredicate fromBlob(const Blob&);
    static TrackPredicate comparisonWith(Common::Property::TypeID, ComparisonOperator, String);
    static TrackPredicate compoundWith(const TrackPredicate&, CompoundOperator, const TrackPredicate&);

    // -- Constructors & Destructors
    ~TrackPredicate() = default;

    // -- Operators
    bool operator==(const TrackPredicate&) const noexcept;
    inline bool operator!=(const TrackPredicate& other) const noexcept
    {
        return !this->operator==(other);
    }

    // -- Instance Methods
    Blob asBlob() const;
};

} }
