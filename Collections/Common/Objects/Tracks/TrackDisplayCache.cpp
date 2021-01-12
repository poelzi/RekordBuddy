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

#include <CommonCollection/Tracks/TrackDisplayCache.hpp>

using namespace NxA;
using namespace NxA::Common;

namespace {

// -- Private Functions

inline const byte* p_nextEntryAfter(const byte* currentEntry)
{
    auto type = static_cast<Common::Property::TypeID>(*currentEntry);
    auto valueType = Common::Property::valueTypeIDFromPropertyType(type);
    if (valueType == Common::Property::OtherValueType) {
        switch (type) {
            case Common::Property::TypeID::RelativeFilePath:
            case Common::Property::TypeID::ArtistCredit:
            case Common::Property::TypeID::ProducerCredit:
            case Common::Property::TypeID::RemixerCredit: {
                valueType = Common::Property::StringValueType;
                break;
            }
            case Common::Property::TypeID::NumberOfTracks: {
                valueType = Common::Property::IntegerValueType;
                break;
            }
            default: {
                NXA_ALOG_WITH_FORMAT("Invalid type 0x%02hhx.", type);
            }
        }
    }

    switch (valueType) {
        case Common::Property::StringValueType: {
            ++currentEntry;

            count sliceSize;

            do {
                sliceSize = *currentEntry++;
                if (!sliceSize) {
                    sliceSize = 256;
                }

                currentEntry += sliceSize;
            } while (sliceSize == 256);

            return currentEntry;
        }
        case Common::Property::IntegerValueType: {
            return currentEntry + 1 + 4;
        }
        case Common::Property::DecimalValueType: {
            return currentEntry + 1 + 8;
        }
        case Common::Property::TimeValueType: {
            return currentEntry + 1 + 8;
        }
        default: {
            NXA_ALOG_WITH_FORMAT("Invalid type 0x%02hhx.", type);
        }
    }
}

void p_walkBlobLookingForTypeIDAndCall(const MutableBlob& blob, Common::Property::TypeID typeID, std::function<void(const byte*)>&& foundOneFunction)
{
    boolean foundOne = false;

    auto currentData = blob.data().get();
    auto endData = currentData + blob.size();
    while (currentData < endData) {
        auto typeFound = static_cast<Common::Property::TypeID>(*currentData);
        if (typeFound != typeID) {
            if (foundOne) {
                // -- Entries are always added by type so if we found at least one and now we don't we don't need to look further.
                break;
            }
        }
        else {
            foundOne = true;

            foundOneFunction(currentData + 1);
        }

        currentData = p_nextEntryAfter(currentData);
    }
}

}

// -- Class Methods

void TrackDisplayCache::addStringValueForTagOfTypeToBlob(const String& value, Common::Property::TypeID typeID, MutableBlob& blob)
{
    NXA_ASSERT_TRUE((Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::StringValueType) ||
                    (typeID == Common::Property::TypeID::RelativeFilePath) || (typeID == Common::Property::TypeID::ArtistCredit) ||
                    (typeID == Common::Property::TypeID::ProducerCredit) || (typeID == Common::Property::TypeID::RemixerCredit));

    // -- This is to make sure the cast below is valid.
    static_assert(sizeof(Common::Property::TypeID) == sizeof(byte), "Common::Property::TypeID needs to be the same size as a byte.");

    blob.append(static_cast<byte>(typeID));

    auto valueData = reinterpret_cast<const byte*>(value.asUTF8());
    count valueDataSize = value.sizeInBytesOfStringAsUTF8();
    NXA_ASSERT_TRUE(valueDataSize != 0);

    count offset = 0;

    do {
        count sliceSize = (valueDataSize >= 256) ? 256 : valueDataSize;

        blob.append(static_cast<byte>(sliceSize));
        blob.appendMemoryWithSize(valueData + offset, sliceSize);

        offset += sliceSize;
        valueDataSize -= sliceSize;
    }
    while(valueDataSize);
}

Array<String> TrackDisplayCache::stringValuesForTagOfTypeInBlob(Common::Property::TypeID typeID, const MutableBlob& blob)
{
    NXA_ASSERT_TRUE((Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::StringValueType) ||
                    (typeID == Common::Property::TypeID::RelativeFilePath) || (typeID == Common::Property::TypeID::ArtistCredit) ||
                    (typeID == Common::Property::TypeID::ProducerCredit) || (typeID == Common::Property::TypeID::RemixerCredit));

    MutableArray<String> results;

    p_walkBlobLookingForTypeIDAndCall(blob, typeID, [&results](const byte* currentData) {
        MutableBlob reconstructedString;
        count sliceSize;

        do {
            sliceSize = *currentData++;
            if (!sliceSize) {
                // -- If the size if zero it means it's really 256 with further slices after it.
                sliceSize = 256;
            }

            reconstructedString.appendMemoryWithSize(currentData, sliceSize);

            currentData += sliceSize;
        }
        while (sliceSize == 256);

        results.append(String::stringWithMemoryAndSizeInBytes(reinterpret_cast<const character*>(reconstructedString.data().get()), reconstructedString.size()));
    });

    return std::move(results);
}

void TrackDisplayCache::addIntegerValueForTagOfTypeToBlob(integer32 value, Common::Property::TypeID typeID, MutableBlob& blob)
{
    NXA_ASSERT_TRUE((Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::IntegerValueType) ||
                    (typeID == Common::Property::TypeID::NumberOfTracks));

    // -- This is to make sure the cast below is valid.
    static_assert(sizeof(Common::Property::TypeID) == sizeof(byte), "Common::Property::TypeID needs to be the same size as a byte.");

    blob.append(static_cast<byte>(typeID));

    uinteger32 asLittleEndian = Platform::fromOrToLittleEndianUInteger32(value);
    blob.appendMemoryWithSize(reinterpret_cast<byte*>(&asLittleEndian), sizeof(asLittleEndian));
}

Array<integer32> TrackDisplayCache::integerValuesForTagOfTypeInBlob(Common::Property::TypeID typeID, const MutableBlob& blob)
{
    NXA_ASSERT_TRUE((Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::IntegerValueType) ||
                    (typeID == Common::Property::TypeID::NumberOfTracks));

    MutableArray<integer32> results;

    p_walkBlobLookingForTypeIDAndCall(blob, typeID, [&results](const byte* currentData) {
        results.append(Platform::fromOrToLittleEndianUInteger32(*reinterpret_cast<const uinteger32*>(currentData)));
    });

    return std::move(results);
}

void TrackDisplayCache::addDecimalValueForTagOfTypeToBlob(const DecimalNumber& value, Common::Property::TypeID typeID, MutableBlob& blob)
{
    NXA_ASSERT_TRUE(Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::DecimalValueType);

    // -- This is to make sure the cast below is valid.
    static_assert(sizeof(Common::Property::TypeID) == sizeof(byte), "Common::Property::TypeID needs to be the same size as a byte.");

    blob.append(static_cast<byte>(typeID));

    uinteger64 packedValue = Platform::fromOrToLittleEndianUInteger64(value.asPackedValue());
    blob.appendMemoryWithSize(reinterpret_cast<const byte*>(&packedValue), sizeof(packedValue));
}

Array<DecimalNumber> TrackDisplayCache::decimalValuesForTagOfTypeInBlob(Common::Property::TypeID typeID, const MutableBlob& blob)
{
    NXA_ASSERT_TRUE(Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::DecimalValueType);

    MutableArray<DecimalNumber> results;

    p_walkBlobLookingForTypeIDAndCall(blob, typeID, [&results](const byte* currentData) {
        results.append(DecimalNumber::withPackedValue(Platform::fromOrToLittleEndianUInteger64(*reinterpret_cast<const uinteger64*>(currentData))));
    });

    return std::move(results);
}

void TrackDisplayCache::addTimeValueForTagOfTypeToBlob(const Time& value, Common::Property::TypeID typeID, MutableBlob& blob)
{
    NXA_ASSERT_TRUE(Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::TimeValueType);

    // -- This is to make sure the cast below is valid.
    static_assert(sizeof(Common::Property::TypeID) == sizeof(byte), "Common::Property::TypeID needs to be the same size as a byte.");

    blob.append(static_cast<byte>(typeID));

    uinteger64 asLittleEndian = Platform::fromOrToLittleEndianUInteger64(value.asUnixTimeStamp());
    blob.appendMemoryWithSize(reinterpret_cast<const byte*>(&asLittleEndian), sizeof(asLittleEndian));
}

Array<Time> TrackDisplayCache::timeValuesForTagOfTypeInBlob(Common::Property::TypeID typeID, const MutableBlob& blob)
{
    NXA_ASSERT_TRUE(Common::Property::valueTypeIDFromPropertyType(typeID) == Common::Property::TimeValueType);

    MutableArray<Time> results;

    p_walkBlobLookingForTypeIDAndCall(blob, typeID, [&results](const byte* currentData) {
        auto maybeNarrowedValue = maybeNarrowCast<timestamp>(Platform::fromOrToLittleEndianUInteger64(*reinterpret_cast<const uinteger64*>(currentData)));
        NXA_ASSERT_TRUE(maybeNarrowedValue.isValid());
        results.append(Time{ *maybeNarrowedValue });
    });

    return std::move(results);
}
