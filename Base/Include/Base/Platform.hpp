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

#include <Base/NotNull.hpp>
#include <Base/Optional.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>
#include <Base/Uncopyable.hpp>

namespace NxA {

class Blob;
class String;

// -- Public Interface
class Platform : private Uncopyable
{
    // -- Private Constants
    union EndianTest {
        uinteger32 integerVersion;
        byte byteVersion[4];
    };

    static inline EndianTest endianTest { 0x01020304 };

protected:
    // -- This can be used during unit tests to force some stubbed values.
    friend class Test;

    // -- Class Variables
#if defined(NXA_BUILD_FOR_TESTING)
    static WeakReference<String> p_testUserName;
#endif

public:
    // -- Constructors & Destructors
    Platform() = delete;

    // -- Constants
    enum class Kind : byte {
        macOS,
        Windows,
        Unknown
    };
    enum class Endian : byte {
        Little,
        Big,
        PDP,
        Unknown
    };

    // -- Class Methods
    static uinteger64 fromOrToLittleEndianUInteger64(uinteger64);
    static uinteger32 fromOrToLittleEndianUInteger32(uinteger32);
    static uinteger16 fromOrToLittleEndianUInteger16(uinteger16);
    static uinteger64 fromOrToBigEndianUInteger64(uinteger64);
    static uinteger32 fromOrToBigEndianUInteger32(uinteger32);
    static uinteger16 fromOrToBigEndianUInteger16(uinteger16);

    static float bigEndianFloatValueAt(NotNull<const void*>);
    static uinteger64 bigEndianUInteger64ValueAt(NotNull<const void*>);
    static uinteger32 bigEndianUInteger32ValueAt(NotNull<const void*>);
    static uinteger16 bigEndianUInteger16ValueAt(NotNull<const void*>);
    static void writeBigEndianFloatValueAt(float, NotNull<void*>);
    static void writeBigEndianUInteger64ValueAt(uinteger64, NotNull<void*>);
    static void writeBigEndianUInteger32ValueAt(uinteger32, NotNull<void*>);
    static void writeBigEndianUInteger16ValueAt(uinteger16, NotNull<void*>);

    static Blob convertEndiannessOfUInteger16From(const Blob&);

    static inline Endian endianOrder()
    {
        return (endianTest.byteVersion[0] == 0x04) ? Platform::Endian::Little :
               (endianTest.byteVersion[0] == 0x01) ? Platform::Endian::Big :
               (endianTest.byteVersion[0] == 0x02) ? Platform::Endian::PDP : Platform::Endian::Unknown;
    }

    static Optional<String> maybeUniqueIDForComputer();
    static Optional<String>& maybeUsername();
    static String osVersion();
    static String hardwareName();

    static boolean isRunningFromInstallationFolder();

    static Array<String> namesOfOtherRunningApplications();

    static void alertWithTextAndInformativeText(const String&, const String&);

#ifdef NXA_PLATFORM_WINDOWS
    static String GetLastError();
#endif
};

}
