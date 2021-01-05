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
#include "Persistence/GenericPersistentObject.hpp"
#include "Persistence/PersistentSQLiteTransaction.hpp"

#define ATTRIBUTE(memberExpr, localType, indexed) \
    Attribute<RBSchema, MemberPointerTraits<decltype(&memberExpr)>::DeclarationType, MemberPointerTraits<decltype(&memberExpr)>::ClassType> \
        { BindableType::AttributeType, ":" #localType, const_cast<std::remove_const_t<decltype(this)>>(this), &memberExpr, AttributeIndexed::indexed }

#define RELATION(memberExpr, localMember, localType, localOrder, localSpan, rule, otherMember, otherOrder, otherSpan) \
    Relationship<RBSchema, MemberPointerTraits<decltype(&memberExpr)>::DeclarationType, MemberPointerTraits<decltype(&memberExpr)>::ClassType, Span::localSpan, Span::otherSpan>{ BindableType::RelationshipType, \
        const_cast<typename MemberPointerTraits<decltype(&memberExpr)>::ClassType*>(this), &memberExpr, ":" #localMember, ":" #otherMember, \
        RBSchema::Type::localType, TypeEnumeration<typename MemberPointerTraits<decltype(&memberExpr)>::ClassType>::value, Rule::rule, Ordering::localOrder, Ordering::otherOrder }

namespace NxA {

enum class Rule : integer8 { Cascade, Nullify };
enum class Mode : integer8 { Save, Load, Declare, Delete, SaveOrdering };
enum class Span : integer8 { ToMany, ToOne, ToOptionalOne };
enum class Ordering : integer8 { Ordered, Unordered };
enum class AttributeIndexed : integer8 { Indexed, Unindexed };
enum class BindableType : integer8 { AttributeType, RelationshipType };

struct Bindable
{
    BindableType bindableType;
    explicit constexpr Bindable(const BindableType& withType) : bindableType{withType} {}
};

template <class Schema, typename T, typename A>
    struct Attribute : Bindable
    {
        const char* localColumnName;
        A* localBasePointer;
        T A::*localMemberPointer;
        AttributeIndexed indexType;
        constexpr Attribute(const BindableType& withBindable, const char* withLocalColumnName, A* withLocalBasePointer, T A::*withLocalMemberPointer, AttributeIndexed withIndexType) : Bindable{withBindable}, localColumnName{withLocalColumnName}, localBasePointer{withLocalBasePointer}, localMemberPointer{withLocalMemberPointer}, indexType{withIndexType} {}
    };

template <class Schema, typename T, typename A>
    using OptionalAttribute = Attribute<Schema, Optional<T>, A>;

template <class Schema, typename T, typename A, Span localSpan, Span otherSpan>
    struct Relationship : Bindable
    {
        static constexpr uinteger16 version = Schema::schemaVersion();
        A* localBasePointer;
        T A::*localMemberPointer;
        const char* localColumnName;
        const char* otherColumnName;
        typename Schema::Type localResultType, otherResultType;
        Rule localCascadeRule;
        Ordering localOrdering, otherOrdering;

        constexpr Relationship(const BindableType& withBindable, A* withLocalBasePointer,
                               T A::*withLocalMemberPointer,
                               const char* withLocalColumnName,
                               const char* withOtherColumnName,
                               typename Schema::Type withLocalResultType,
                               typename Schema::Type withOtherResultType,
                               Rule withLocalCascadeRule,
                               Ordering withLocalOrdering,
                               Ordering withOtherOrdering) :
            Bindable{withBindable},
            localBasePointer{withLocalBasePointer},
            localMemberPointer{withLocalMemberPointer},
            localColumnName{withLocalColumnName},
            otherColumnName{withOtherColumnName},
            localResultType{withLocalResultType},
            otherResultType{withOtherResultType},
            localCascadeRule{withLocalCascadeRule},
            localOrdering{withLocalOrdering},
            otherOrdering{withOtherOrdering}
        { }
    };

template <class Schema, typename T, typename A, Span otherSpan>
    using OptionalRelationship = Relationship<Schema, Optional<T>, A, Span::ToOptionalOne, otherSpan>;

template <class Schema, typename T, typename A, Span localSpan, Span otherSpan>
    constexpr bool operator<(const Relationship<Schema, T, A, localSpan, otherSpan> &lhs, const Relationship<Schema, T, A, localSpan, otherSpan> &rhs)
    {
        return lhs.version < rhs.version ||
            lhs.localResultType < rhs.localResultType ||
            lhs.localColumnName < rhs.localColumnName ||
            lhs.localCascadeRule < rhs.localCascadeRule ||
            lhs.localOrdering < rhs.localOrdering ||
            lhs.otherColumnName < rhs.otherColumnName ||
            lhs.otherResultType < rhs.otherResultType ||
            lhs.otherOrdering < rhs.otherOrdering;
    }

}
