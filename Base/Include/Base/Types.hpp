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

#include <ctime>
#include <cstdint>
#include <memory>
#include <typeinfo>
#include <limits>

namespace NxA {

// -- Types used by the codebase.
using boolean = bool;

using uinteger = unsigned int;
using integer = int;
using uinteger8 = uint8_t;
using integer8 = int8_t;
using uinteger16 = uint16_t;
using integer16 = int16_t;
using uinteger32 = uint32_t;
using integer32 = int32_t;
using uinteger64 = uint64_t;
using integer64 = int64_t;

using byte = uint8_t;
using character = char;

using count = uint64_t;
static_assert(!std::numeric_limits<count>::is_signed, "This platform does not store sizes as unsigned values.");

using timestamp = integer64;

// -- Declarations marked with DEPRECATED will still work but get marked with a warning. Useful for doing refactorings with the help of name resolution.
#define DEPRECATED __attribute__ ((deprecated))

// -- The compiler will inline declarations marked with ALWAYS_INLINE
#define ALWAYS_INLINE __attribute__((always_inline))

// -- function declarations with WARN_UNUSED_RESULT get the compiler to require caller of function to capture return value
#define WARN_UNUSED_RESULT __attribute__((warn_unused_result))

// -- Member function pointer trait to get the class type and declaration type
template <class MP>
    struct MemberPointerTraitsHelper;

template <class T, class U>
    struct MemberPointerTraitsHelper<T U::*>
    {
        using ClassType = U;
        using DeclarationType = T;
    };

template <class MP>
    struct MemberPointerTraits : MemberPointerTraitsHelper<typename std::remove_cv<MP>::type>
    {
    };

// -- Can be used to convert enums to their integral values
template<typename E>
    constexpr auto enumToInteger(E e) -> typename std::underlying_type<E>::type
    {
        return static_cast<typename std::underlying_type<E>::type>(e);
    }

// -- Forward declaration used to make our test classes friends of NXA objects.
class Test;

// -- We want to make sure the compiler supports UTF-8 correctly.
// -- This test is buggy right now on Windows. To be revisited.
//static_assert(
//    (static_cast<unsigned char>("🦂"[0]) == 0xf0) &&
//    (static_cast<unsigned char>("🦂"[1]) == 0x9f) &&
//    (static_cast<unsigned char>("🦂"[2]) == 0xa6) &&
//    (static_cast<unsigned char>("🦂"[3]) == 0x82), "Compiler is buggy with UTF-8. specify `/utf-8` for Visual Studio");

template <class T>
    class Array;

template <class T>
    class MutableArray;

}
