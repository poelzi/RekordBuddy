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

#include <Base/Array.hpp>
#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>

namespace NxA { namespace Common {

// -- Forward Declarations
class MutableTrack;
class Track;
class MutableCollection;
class Collection;

// -- Public Interface
class MusicalKey
{
public:
    // -- Constants
    enum class Value : uinteger32 {
        C,
        CsDb,
        D,
        DsEb,
        E,
        F,
        FsGb,
        G,
        GsAb,
        A,
        AsBb,
        B,
        Cm,
        CsmDbm,
        Dm,
        DsmEbm,
        Em,
        Fm,
        FsmGbm,
        Gm,
        GsmAbm,
        Am,
        AsmBbm,
        Bm,

        Maximum = Bm,
    };

    enum class Notation : uinteger32 {
        // -- These are stored in user data and should not be modified.
        Standard,             // -- Standard musical notation.
        StandardAllSharps,    // -- Standard musical notation with only sharps.
        Camelot,              // -- Camelot musical notation (used by Mixed-in-key).
        CamelotLeadingZero,   // -- Camelot musical notation (used by Mixed-in-key) with values like 01A instead of 1A.
        OpenKey,              // -- OpenKey musical notation.
        OpenKeyLeadingZero,   // -- OpenKey musical notation with values like 01m instead of 1m.

        Maximum = OpenKeyLeadingZero,
    };

    // -- Class Methods
    static Array<String> keyNamesStartingWith(const String&);
    static Array<Value> allKeyValues();

    static Common::MusicalKey::Notation defaultNotation();
    static void setDefaultNotation(Common::MusicalKey::Notation);

    static String stringValueForKey(Common::MusicalKey::Value);
    static String stringValueForKeyUsingNotation(Common::MusicalKey::Value, Common::MusicalKey::Notation);
    static Optional<Common::MusicalKey::Value> maybeKeyValueFromString(const String&);
    static Optional<String> maybeStringValueInDefaultNotationFromString(const String&);

    // -- Constructors & Destructors
    virtual ~MusicalKey() = default;

    // -- Operators
    inline bool operator==(const Common::MusicalKey& other) const noexcept
    {
        return this->value() == other.value();
    }
    inline bool operator!=(const Common::MusicalKey& other) const noexcept
    {
        return !this->operator==(other);
    }
    inline bool operator<(const Common::MusicalKey& other) const noexcept
    {
        return this->value() < other.value();
    }

    // -- Instance Methods
    virtual NotNull<const Collection*> collection() const = 0;

    virtual Value value() const = 0;
    virtual String stringValue() const = 0;

    virtual Array<NotNull<const Track*>> tracks() const = 0;
};

class MutableMusicalKey : public MusicalKey
{
public:
    // -- Constructors & Destructors
    virtual ~MutableMusicalKey() = default;

    // -- Instance Methods
    virtual NotNull<MutableCollection*> collection() = 0;

    virtual Array<NotNull<MutableTrack*>> tracks() = 0;

    // -- Overriden MusicalKey Instance Methods
    NotNull<const Collection*> collection() const override = 0;

    Value value() const override = 0;
    String stringValue() const override = 0;

    Array<NotNull<const Track*>> tracks() const override = 0;
};

} }
