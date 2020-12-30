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

#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/NotNull.hpp>
#include <Base/Platform.hpp>
#include <Base/Types.hpp>

#include <cstdint>
#include <type_traits>

using namespace NxA;

// -- Class Variables

#if defined(NXA_BUILD_FOR_TESTING)
WeakReference<String> Platform::p_testUserName;
#endif

// -- Class Methods

uinteger64 Platform::fromOrToLittleEndianUInteger64(uinteger64 value)
{
    if (Platform::endianOrder() == Platform::Endian::Little) {
        return value;
    }
    else if (Platform::endianOrder() == Platform::Endian::Big) {
        uinteger64 result = 0;

        const auto* charsPtr = reinterpret_cast<const character*>(&value);
        for (count index = 0; index <= 7; ++index) {
            result <<= 8;
            result |= charsPtr[index] & 0xff;
        }

        return result;
    }
    else {
        NXA_ALOG("Not Implemented.");
    }
}

uinteger32 Platform::fromOrToLittleEndianUInteger32(uinteger32 value)
{
    if (Platform::endianOrder() == Platform::Endian::Little) {
        return value;
    }
    else if (Platform::endianOrder() == Platform::Endian::Big) {
        uinteger32 result = 0;

        const auto* charsPtr = reinterpret_cast<const character*>(&value);
        for (count index = 0; index <= 3; ++index) {
            result <<= 8;
            result |= charsPtr[index] & 0xff;
        }

        return result;
    }
    else {
        NXA_ALOG("Not Implemented.");
    }
}

uinteger16 Platform::fromOrToLittleEndianUInteger16(uinteger16 value)
{
    if (Platform::endianOrder() == Platform::Endian::Little) {
        return value;
    }
    else if (Platform::endianOrder() == Platform::Endian::Big) {
        uinteger16 result = 0;

        const auto* charsPtr = reinterpret_cast<const character*>(&value);
        for (count index = 0; index <= 1; ++index) {
            result <<= 8;
            result |= charsPtr[index] & 0xff;
        }

        return result;
    }
    else {
        NXA_ALOG("Not Implemented.");
    }
}

uinteger64 Platform::fromOrToBigEndianUInteger64(uinteger64 value)
{
    if (Platform::endianOrder() == Platform::Endian::Little) {
        uinteger64 result = 0;

        const auto* charsPtr = reinterpret_cast<const character*>(&value);
        for (count index = 0; index <= 7; ++index) {
            result <<= 8;
            result |= charsPtr[index] & 0xff;
        }

        return result;
    }
    else if (Platform::endianOrder() == Platform::Endian::Big) {
        return value;
    }
    else {
        NXA_ALOG("Not Implemented.");
    }
}

uinteger32 Platform::fromOrToBigEndianUInteger32(uinteger32 value)
{
    if (Platform::endianOrder() == Platform::Endian::Little) {
        uinteger32 result = 0;

        const auto* charsPtr = reinterpret_cast<const character*>(&value);
        for (count index = 0; index <= 3; ++index) {
            result <<= 8;
            result |= charsPtr[index] & 0xff;
        }

        return result;
    }
    else if (Platform::endianOrder() == Platform::Endian::Big) {
        return value;
    }
    else {
        NXA_ALOG("Not Implemented.");
    }
}

uinteger16 Platform::fromOrToBigEndianUInteger16(uinteger16 value)
{
    if (Platform::endianOrder() == Platform::Endian::Little) {
        uinteger16 result = 0;

        const auto* charsPtr = reinterpret_cast<const character*>(&value);
        for (count index = 0; index <= 1; ++index) {
            result <<= 8;
            result |= charsPtr[index] & 0xff;
        }

        return result;
    }
    else if (Platform::endianOrder() == Platform::Endian::Big) {
        return value;
    }
    else {
        NXA_ALOG("Not Implemented.");
    }
}

float Platform::bigEndianFloatValueAt(NotNull<const void*> pointer)
{
    const auto* charsPtr = reinterpret_cast<const character*>(pointer.get());
    int32_t bigEndianVersion = ((charsPtr[0] << 24) & 0xff000000) | ((charsPtr[1] << 16) & 0xff0000) | ((charsPtr[2] << 8) & 0xff00) | (charsPtr[3] & 0xff);
    return *reinterpret_cast<float*>(&bigEndianVersion);
}

uinteger64 Platform::bigEndianUInteger64ValueAt(NotNull<const void*> pointer)
{
    uinteger64 result = 0;

    const auto* charsPtr = reinterpret_cast<const character*>(pointer.get());
    for (count index = 0; index <= 7; ++index) {
        result <<= 8;
        result |= charsPtr[index] & 0xff;
    }

    return result;
}

uinteger32 Platform::bigEndianUInteger32ValueAt(NotNull<const void*> pointer)
{
    uinteger32 result = 0;

    const auto* charsPtr = reinterpret_cast<const character*>(pointer.get());
    for (count index = 0; index <= 3; ++index) {
        result <<= 8;
        result |= charsPtr[index] & 0xff;
    }

    return result;
}

uinteger16 Platform::bigEndianUInteger16ValueAt(NotNull<const void*> pointer)
{
    const auto* charsPtr = reinterpret_cast<const character*>(pointer.get());
    return ((charsPtr[0] << 8) & 0xff00) | (charsPtr[1] & 0xff);
}

void Platform::writeBigEndianFloatValueAt(float value, NotNull<void*> pointer)
{
    auto* charsPtr = reinterpret_cast<character*>(pointer.get());
    const auto* valuePtr = reinterpret_cast<const character*>(&value);
    charsPtr[0] = valuePtr[3];
    charsPtr[1] = valuePtr[2];
    charsPtr[2] = valuePtr[1];
    charsPtr[3] = valuePtr[0];
}

void Platform::writeBigEndianUInteger64ValueAt(uinteger64 value, NotNull<void*> pointer)
{
    auto* charsPtr = reinterpret_cast<character*>(pointer.get());
    for (count index = 0; index <= 7; ++index) {
        charsPtr[7 - index] = value & 0xff;
        value >>= 8;
    }
}

void Platform::writeBigEndianUInteger32ValueAt(uinteger32 value, NotNull<void*> pointer)
{
    auto* charsPtr = reinterpret_cast<character*>(pointer.get());
    for (count index = 0; index <= 3; ++index) {
        charsPtr[3 - index] = value & 0xff;
        value >>= 8;
    }
}

void Platform::writeBigEndianUInteger16ValueAt(uinteger16 value, NotNull<void*> pointer)
{
    auto* charsPtr = reinterpret_cast<character*>(pointer.get());
    charsPtr[0] = (value >> 8) & 0xff;
    charsPtr[1] = value & 0xff;
}

Blob Platform::convertEndiannessOfUInteger16From(const Blob& other)
{
    MutableBlob copy(other);
    if (!copy.size()) {
        return std::move(copy);
    }

    auto numberOfSwaps = copy.size() / 2;

    integer index = 0;
    auto data = copy.data();
    for (count c = 0; c < numberOfSwaps; ++c, index += 2) {
        std::swap(data.get()[index], data.get()[index + 1]);
    }

    return std::move(copy);
}
