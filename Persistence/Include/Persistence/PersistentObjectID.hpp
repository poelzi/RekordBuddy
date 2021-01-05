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

// -- ensure we're packed as tight as possible even on windows platform
#pragma pack(push,1)

// Uniquely identifies objects in the database.
template <typename Schema>
struct PersistentObjectID
{
    using Type = typename Schema::Type;
    using Index = integer64;
    using Version = uinteger16;
    static_assert(Schema::schemaVersion() > 0, "version must be positive");

    // -- Constructors

    PersistentObjectID(const PersistentObjectID&) = default;
    PersistentObjectID(PersistentObjectID&&) noexcept = default;
    PersistentObjectID& operator=(const PersistentObjectID&) = default;
    PersistentObjectID& operator=(PersistentObjectID&&) noexcept = default;
    ~PersistentObjectID() = default;

    constexpr PersistentObjectID() : index{}, type{}
    {
    }

    constexpr PersistentObjectID(Index withIndex, Type withType) : index{withIndex}, type{withType}
    {
        static_assert(sizeof(PersistentObjectID<Schema>) <= 16, "ObjectID must be 8 bytes");

        // -- checking that input matches stored data as a quick way to check for overflow.
        NXA_ASSERT_TRUE(this->isValid());
    }

    explicit constexpr PersistentObjectID(Type withType) : index{}, type{withType}
    {
        NXA_ASSERT_TRUE(this->isValid());
    }

    template <typename T>
    constexpr static PersistentObjectID<Schema> idWithIndex(Index index)
    {
        auto type = Schema::template TypeEnumeration<T>::value;
        static_assert(Schema::template TypeEnumeration<T>::value != Schema::Type::Undefined_, "Type must be defined in current Schema");
        return {index, type};
    }

#if DEBUG
    String description() const
    {
        return String::stringWithFormat("PersistentObjectID type=%s index=%d version=%d", Schema::typeToString(this->objectType()), this->objectIndex(), Schema::schemaVersion());
    }
#endif

    inline constexpr boolean isValid() const
    {
        return (type >= Type::First_) && (type < Type::Last_);
    }

    // -- Operators
    inline constexpr integer32 compareTo(PersistentObjectID<Schema> other) const
    {
        if (type != other.type) {
            return (type < other.type) ? -1 : 1;
        }

        if (index != other.index) {
            return (index < other.index) ? -1 : 1;
        }

        return 0;
    }

    inline constexpr Type objectType() const
    {
        return type;
    }

    inline constexpr Index objectIndex() const
    {
        return index;
    }

    inline constexpr Version objectSchemaVersion() const
    {
        return Schema::schemaVersion();
    }

    // -- Operators
    inline constexpr bool operator<(PersistentObjectID other) const
    {
        return compareTo(other) < 0;
    }

    inline constexpr bool operator>(PersistentObjectID other) const
    {
        return compareTo(other) > 0;
    }

    inline constexpr bool operator<=(PersistentObjectID other) const
    {
        return compareTo(other) <= 0;
    }

    inline constexpr bool operator>=(PersistentObjectID other) const
    {
        return compareTo(other) >= 0;
    }

    inline constexpr bool operator==(PersistentObjectID other) const
    {
        return compareTo(other) == 0;
    }

    inline constexpr bool operator!=(PersistentObjectID other) const
    {
        return compareTo(other) != 0;
    }

    template <typename Char, typename CharTraits>
        friend inline ::std::basic_ostream<Char, CharTraits>& operator<<(::std::basic_ostream<Char, CharTraits>& os,
                                                                         const PersistentObjectID<Schema>& self)
        {
            os << "<PersistentObjectID type=\"" << Schema::typeToString(self.objectType()) << "\" index=\"" << self.objectIndex() << "\" version=\""
               << Schema::schemaVersion() << "\" />";
            return os;
        }

private:
    Index index : 52;
    Type type : 12;
    static_assert(static_cast<integer64>(Type::Last_) + 2 < (1 << 12), "Type must fit inside 12 bit unsigned integer (with 2 reserved slots to work around limit if it's reached)");
};

#pragma pack(pop)

template <typename Schema>
constexpr PersistentObjectID<Schema> badID{};

}

namespace std {
template <typename Schema>
struct hash<NxA::PersistentObjectID<Schema>>
{
    constexpr std::size_t operator()(const NxA::PersistentObjectID<Schema>& k) const
    {
        static_assert(sizeof(NxA::PersistentObjectID<Schema>) <= 64, "Object ID must be able to fit in a machine word");
        using std::size_t;
        using std::hash;
        using Index = typename NxA::PersistentObjectID<Schema>::Index;
        using Type = typename NxA::PersistentObjectID<Schema>::Type;
        using Version = typename NxA::PersistentObjectID<Schema>::Version;
        size_t hashResult = (5381 << 16) + 5381;
        hashResult = ((hashResult << 5) + hashResult) ^ hash<Index>{}(k.objectIndex());
        hashResult = ((hashResult << 5) + hashResult + (hashResult >> 27)) ^ hash<Type>{}(k.objectType());
        hashResult = ((hashResult << 5) + hashResult + (hashResult >> 27)) ^ hash<Version>{}(k.objectSchemaVersion());
        return hashResult;
    }
};
}
