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

#include <CommonCollection/Tracks/MusicalKey.hpp>
#include <CommonCollection/Tracks/Track.hpp>

namespace NxA { namespace Common {

// -- Translation to musical keys for the regular musical notation.

static Map<MusicalKey::Value, String> p_musicalNotationTranslation
{
    { MusicalKey::Value::C, "C" },
    { MusicalKey::Value::CsDb, "Db"},
    { MusicalKey::Value::D, "D" },
    { MusicalKey::Value::DsEb, "Eb" },
    { MusicalKey::Value::E, "E" },
    { MusicalKey::Value::F, "F" },
    { MusicalKey::Value::FsGb, "Gb" },
    { MusicalKey::Value::G, "G" },
    { MusicalKey::Value::GsAb, "Ab" },
    { MusicalKey::Value::A, "A" },
    { MusicalKey::Value::AsBb, "Bb" },
    { MusicalKey::Value::B, "B" },
    { MusicalKey::Value::Cm, "Cm" },
    { MusicalKey::Value::CsmDbm, "Dbm" },
    { MusicalKey::Value::Dm, "Dm" },
    { MusicalKey::Value::DsmEbm, "Ebm" },
    { MusicalKey::Value::Em, "Em" },
    { MusicalKey::Value::Fm, "Fm" },
    { MusicalKey::Value::FsmGbm, "Gbm" },
    { MusicalKey::Value::Gm, "Gm" },
    { MusicalKey::Value::GsmAbm, "Abm" },
    { MusicalKey::Value::Am, "Am" },
    { MusicalKey::Value::AsmBbm, "Bbm" },
    { MusicalKey::Value::Bm, "Bm" },
};

// -- Translation to musical keys for the regular musical notation with sharps instead of flats.

static Map<MusicalKey::Value, String> p_musicalNotationAllSharpsTranslation
{
    { MusicalKey::Value::C, "C" },
    { MusicalKey::Value::CsDb, "C#" },
    { MusicalKey::Value::D, "D" },
    { MusicalKey::Value::DsEb, "D#" },
    { MusicalKey::Value::E, "E" },
    { MusicalKey::Value::F, "F" },
    { MusicalKey::Value::FsGb, "F#" },
    { MusicalKey::Value::G, "G" },
    { MusicalKey::Value::GsAb, "G#" },
    { MusicalKey::Value::A, "A" },
    { MusicalKey::Value::AsBb, "A#" },
    { MusicalKey::Value::B, "B" },
    { MusicalKey::Value::Cm, "Cm" },
    { MusicalKey::Value::CsmDbm, "C#m" },
    { MusicalKey::Value::Dm, "Dm" },
    { MusicalKey::Value::DsmEbm, "D#m" },
    { MusicalKey::Value::Em, "Em" },
    { MusicalKey::Value::Fm, "Fm" },
    { MusicalKey::Value::FsmGbm, "F#m" },
    { MusicalKey::Value::Gm, "Gm" },
    { MusicalKey::Value::GsmAbm, "G#m" },
    { MusicalKey::Value::Am, "Am" },
    { MusicalKey::Value::AsmBbm, "A#m" },
    { MusicalKey::Value::Bm, "Bm" },
};

// -- Translation to musical keys for an alternative regular musical notation.

static Map<MusicalKey::Value, String> p_otherMusicalNotationTranslation
{
    { MusicalKey::Value::C, "Cmaj" },
    { MusicalKey::Value::CsDb, "Dbmaj" },
    { MusicalKey::Value::D, "Dmaj" },
    { MusicalKey::Value::DsEb, "Ebmaj" },
    { MusicalKey::Value::E, "Emaj" },
    { MusicalKey::Value::F, "Fmaj" },
    { MusicalKey::Value::FsGb, "Gbmaj" },
    { MusicalKey::Value::G, "Gmaj" },
    { MusicalKey::Value::GsAb, "Abmaj" },
    { MusicalKey::Value::A, "Amaj" },
    { MusicalKey::Value::AsBb, "Bbmaj" },
    { MusicalKey::Value::B, "Bmaj" },
    { MusicalKey::Value::Cm, "Cmin" },
    { MusicalKey::Value::CsmDbm, "Dbmin" },
    { MusicalKey::Value::Dm, "Dmin" },
    { MusicalKey::Value::DsmEbm, "Ebmin" },
    { MusicalKey::Value::Em, "Emin" },
    { MusicalKey::Value::Fm, "Fmin" },
    { MusicalKey::Value::FsmGbm, "Gbmin" },
    { MusicalKey::Value::Gm, "Gmin" },
    { MusicalKey::Value::GsmAbm, "Abmin" },
    { MusicalKey::Value::Am, "Amin" },
    { MusicalKey::Value::AsmBbm, "Bbmin" },
    { MusicalKey::Value::Bm, "Bmin" },
};

// -- Translation to musical keys for an alternative regular musical notation with sharps instead of flats.

static Map<MusicalKey::Value, String> p_otherMusicalNotationAllSharpsTranslation
{
    { MusicalKey::Value::CsDb, "C#maj" },
    { MusicalKey::Value::DsEb, "D#maj" },
    { MusicalKey::Value::FsGb, "F#maj" },
    { MusicalKey::Value::GsAb, "G#maj" },
    { MusicalKey::Value::AsBb, "A#maj" },
    { MusicalKey::Value::CsmDbm, "C#min" },
    { MusicalKey::Value::DsmEbm, "D#min" },
    { MusicalKey::Value::FsmGbm, "F#min" },
    { MusicalKey::Value::GsmAbm, "G#min" },
    { MusicalKey::Value::AsmBbm, "A#min" },
};

// -- Translation to musical keys for the open key musical notation.

static Map<MusicalKey::Value, String> p_openKeyNotationTranslation
{
    { MusicalKey::Value::C, "1d" },
    { MusicalKey::Value::CsDb, "8d" },
    { MusicalKey::Value::D, "3d" },
    { MusicalKey::Value::DsEb, "10d" },
    { MusicalKey::Value::E, "5d" },
    { MusicalKey::Value::F, "12d" },
    { MusicalKey::Value::FsGb, "7d" },
    { MusicalKey::Value::G, "2d" },
    { MusicalKey::Value::GsAb, "9d" },
    { MusicalKey::Value::A, "4d" },
    { MusicalKey::Value::AsBb, "11d" },
    { MusicalKey::Value::B, "6d" },
    { MusicalKey::Value::Cm, "10m" },
    { MusicalKey::Value::CsmDbm, "5m" },
    { MusicalKey::Value::Dm, "12m" },
    { MusicalKey::Value::DsmEbm, "7m" },
    { MusicalKey::Value::Em, "2m" },
    { MusicalKey::Value::Fm, "9m" },
    { MusicalKey::Value::FsmGbm, "4m" },
    { MusicalKey::Value::Gm, "11m" },
    { MusicalKey::Value::GsmAbm, "6m" },
    { MusicalKey::Value::Am, "1m" },
    { MusicalKey::Value::AsmBbm, "8m" },
    { MusicalKey::Value::Bm, "3m" },
};

// -- Translation to musical keys for the open key musical notation with leading zeros.

static Map<MusicalKey::Value, String> p_openKeyNotationLeadingZerosTranslation
{
    { MusicalKey::Value::C, "01d" },
    { MusicalKey::Value::CsDb, "08d" },
    { MusicalKey::Value::D, "03d" },
    { MusicalKey::Value::DsEb, "10d" },
    { MusicalKey::Value::E, "05d" },
    { MusicalKey::Value::F, "12d" },
    { MusicalKey::Value::FsGb, "07d" },
    { MusicalKey::Value::G, "02d" },
    { MusicalKey::Value::GsAb, "09d" },
    { MusicalKey::Value::A, "04d" },
    { MusicalKey::Value::AsBb, "11d" },
    { MusicalKey::Value::B, "06d" },
    { MusicalKey::Value::Cm, "10m" },
    { MusicalKey::Value::CsmDbm, "05m" },
    { MusicalKey::Value::Dm, "12m" },
    { MusicalKey::Value::DsmEbm, "07m" },
    { MusicalKey::Value::Em, "02m" },
    { MusicalKey::Value::Fm, "09m" },
    { MusicalKey::Value::FsmGbm, "04m" },
    { MusicalKey::Value::Gm, "11m" },
    { MusicalKey::Value::GsmAbm, "06m" },
    { MusicalKey::Value::Am, "01m" },
    { MusicalKey::Value::AsmBbm, "08m" },
    { MusicalKey::Value::Bm, "03m" },
};

// -- Translation to musical keys for the camelot musical notation.

static Map<MusicalKey::Value, String> p_camelotNotationTranslation
{
    { MusicalKey::Value::C, "8B" },
    { MusicalKey::Value::CsDb, "3B" },
    { MusicalKey::Value::D, "10B" },
    { MusicalKey::Value::DsEb, "5B" },
    { MusicalKey::Value::E, "12B" },
    { MusicalKey::Value::F, "7B" },
    { MusicalKey::Value::FsGb, "2B" },
    { MusicalKey::Value::G, "9B" },
    { MusicalKey::Value::GsAb, "4B" },
    { MusicalKey::Value::A, "11B" },
    { MusicalKey::Value::AsBb, "6B" },
    { MusicalKey::Value::B, "1B" },
    { MusicalKey::Value::Cm, "5A" },
    { MusicalKey::Value::CsmDbm, "12A" },
    { MusicalKey::Value::Dm, "7A" },
    { MusicalKey::Value::DsmEbm, "2A" },
    { MusicalKey::Value::Em, "9A" },
    { MusicalKey::Value::Fm, "4A" },
    { MusicalKey::Value::FsmGbm, "11A" },
    { MusicalKey::Value::Gm, "6A" },
    { MusicalKey::Value::GsmAbm, "1A" },
    { MusicalKey::Value::Am, "8A" },
    { MusicalKey::Value::AsmBbm, "3A" },
    { MusicalKey::Value::Bm, "10A" },
};

// -- Translation to musical keys for the camelot musical notation with leading zeros.

static Map<MusicalKey::Value, String> p_camelotNotationLeadingZerosTranslation
{
    { MusicalKey::Value::C, "08B" },
    { MusicalKey::Value::CsDb, "03B" },
    { MusicalKey::Value::D, "10B" },
    { MusicalKey::Value::DsEb, "05B" },
    { MusicalKey::Value::E, "12B" },
    { MusicalKey::Value::F, "07B" },
    { MusicalKey::Value::FsGb, "02B" },
    { MusicalKey::Value::G, "09B" },
    { MusicalKey::Value::GsAb, "04B" },
    { MusicalKey::Value::A, "11B" },
    { MusicalKey::Value::AsBb, "06B" },
    { MusicalKey::Value::B, "01B" },
    { MusicalKey::Value::Cm, "05A" },
    { MusicalKey::Value::CsmDbm, "12A" },
    { MusicalKey::Value::Dm, "07A" },
    { MusicalKey::Value::DsmEbm, "02A" },
    { MusicalKey::Value::Em, "09A" },
    { MusicalKey::Value::Fm, "04A" },
    { MusicalKey::Value::FsmGbm, "11A" },
    { MusicalKey::Value::Gm, "06A" },
    { MusicalKey::Value::GsmAbm, "01A" },
    { MusicalKey::Value::Am, "08A" },
    { MusicalKey::Value::AsmBbm, "03A" },
    { MusicalKey::Value::Bm, "10A" },
};

static Array<MusicalKey::Value> p_keys
{
    MusicalKey::Value::C,
    MusicalKey::Value::CsDb,
    MusicalKey::Value::D,
    MusicalKey::Value::DsEb,
    MusicalKey::Value::E,
    MusicalKey::Value::F,
    MusicalKey::Value::FsGb,
    MusicalKey::Value::G,
    MusicalKey::Value::GsAb,
    MusicalKey::Value::A,
    MusicalKey::Value::AsBb,
    MusicalKey::Value::B,
    MusicalKey::Value::Cm,
    MusicalKey::Value::CsmDbm,
    MusicalKey::Value::Dm,
    MusicalKey::Value::DsmEbm,
    MusicalKey::Value::Em,
    MusicalKey::Value::Fm,
    MusicalKey::Value::FsmGbm,
    MusicalKey::Value::Gm,
    MusicalKey::Value::GsmAbm,
    MusicalKey::Value::Am,
    MusicalKey::Value::AsmBbm,
    MusicalKey::Value::Bm,
};

// -- Private Methods

static const MutableMap<String, MusicalKey::Value>& p_keyValueFromNameTranslationTable()
{
    static Optional<MutableMap<String, MusicalKey::Value>> keyValueFromNameTranslationTable;
    if (!keyValueFromNameTranslationTable.isValid()) {
        keyValueFromNameTranslationTable = MutableMap<String, MusicalKey::Value>();

        auto sourceTables = Array<Map<MusicalKey::Value, String>>{
                p_musicalNotationTranslation,
                p_musicalNotationAllSharpsTranslation,
                p_otherMusicalNotationTranslation,
                p_otherMusicalNotationAllSharpsTranslation,
                p_openKeyNotationTranslation,
                p_openKeyNotationLeadingZerosTranslation,
                p_camelotNotationTranslation,
                p_camelotNotationLeadingZerosTranslation,
        };

        for (auto&& sourceTable : sourceTables) {
            for (auto&& keyNameTranslation : sourceTable) {
                keyValueFromNameTranslationTable->setValueForKey(keyNameTranslation.first, keyNameTranslation.second.lowerCaseString());
            }
        }
    }

    return *keyValueFromNameTranslationTable;
}

// -- Private Variables

static MusicalKey::Notation p_defaultKeyNotation = MusicalKey::Notation::Standard;

} }

using namespace NxA;
using namespace NxA::Common;

// -- Implementation

// -- Class Methods

Array<MusicalKey::Value> MusicalKey::allKeyValues()
{
    return p_keys;
}

Array<String> MusicalKey::keyNamesStartingWith(const String& prefix)
{
    MutableArray<String> results;

    auto prefixLowerCase = prefix.lowerCaseString();

    for (auto&& keyNameAndValue : p_keyValueFromNameTranslationTable()) {
        auto& keyName = keyNameAndValue.first;

        if (keyName.hasPrefix(prefixLowerCase)) {
            results.append(keyName);
        }
    }

    return std::move(results);
}

Common::MusicalKey::Notation MusicalKey::defaultNotation()
{
    return p_defaultKeyNotation;
}

void MusicalKey::setDefaultNotation(MusicalKey::Notation notation)
{
    p_defaultKeyNotation = notation;
}

String MusicalKey::stringValueForKey(MusicalKey::Value key)
{
    return MusicalKey::stringValueForKeyUsingNotation(key, p_defaultKeyNotation);
}

String MusicalKey::stringValueForKeyUsingNotation(MusicalKey::Value key, MusicalKey::Notation notation)
{
    switch (notation) {
        case Common::MusicalKey::Notation::OpenKey: {
            auto maybeStringValue = p_openKeyNotationTranslation.maybeValueForKey(key);
            NXA_ASSERT_TRUE(maybeStringValue.isValid());
            return *maybeStringValue;
        }
        case Common::MusicalKey::Notation::Camelot: {
            auto maybeStringValue = p_camelotNotationTranslation.maybeValueForKey(key);
            NXA_ASSERT_TRUE(maybeStringValue.isValid());
            return *maybeStringValue;
        }
        case Common::MusicalKey::Notation::OpenKeyLeadingZero: {
            auto maybeStringValue = p_openKeyNotationLeadingZerosTranslation.maybeValueForKey(key);
            NXA_ASSERT_TRUE(maybeStringValue.isValid());
            return *maybeStringValue;
        }
        case Common::MusicalKey::Notation::CamelotLeadingZero: {
            auto maybeStringValue = p_camelotNotationLeadingZerosTranslation.maybeValueForKey(key);
            NXA_ASSERT_TRUE(maybeStringValue.isValid());
            return *maybeStringValue;
        }
        case Common::MusicalKey::Notation::StandardAllSharps: {
            auto maybeStringValue = p_musicalNotationAllSharpsTranslation.maybeValueForKey(key);
            NXA_ASSERT_TRUE(maybeStringValue.isValid());
            return *maybeStringValue;
        }
        default: {
            auto maybeStringValue = p_musicalNotationTranslation.maybeValueForKey(key);
            NXA_ASSERT_TRUE(maybeStringValue.isValid());
            return *maybeStringValue;
        }
    }
}

Optional<MusicalKey::Value> MusicalKey::maybeKeyValueFromString(const String& name)
{
    MutableString nameWithoutSpaces{name.stringByReplacingOccurencesOfWith(" ", "")};
    if (nameWithoutSpaces.isEmpty()) {
        return nothing;
    }

    if (nameWithoutSpaces.hasPostfix("M")) {
        nameWithoutSpaces.append("aj");
    }

    return p_keyValueFromNameTranslationTable().maybeValueForKey(name.lowerCaseString());
}

Optional<String> MusicalKey::maybeStringValueInDefaultNotationFromString(const String& name)
{
    auto maybeKeyValue = Common::MusicalKey::maybeKeyValueFromString(name);
    return maybeKeyValue.isValid() ? Common::MusicalKey::stringValueForKey(*maybeKeyValue) : Optional<String>{ };
}