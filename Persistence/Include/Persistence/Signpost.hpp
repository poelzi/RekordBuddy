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

#if defined(__cplusplus)
namespace NxA {
enum class SignpostColor : short { Blue = 0, Green = 1, Purple = 2, Orange = 3, Red = 4 };

#if !defined(DEBUG) && !defined(PROFILE)
#undef NXA_SKIP_COCOA_PROFILING_SIGNPOSTS
#define NXA_SKIP_COCOA_PROFILING_SIGNPOSTS
#endif

#if !defined(NXA_SKIP_COCOA_PROFILING_SIGNPOSTS)
#include <sys/kdebug_signpost.h>
#endif
// Signposts are a lightweight way to make using instruments a lot easier
// Usage: Signpost name{0}; to put the current scope into a signpost pair.
struct Signpost
{
    unsigned long arg1, arg2, arg3;
    SignpostColor color;

    Signpost() = delete;
    Signpost(const Signpost&) = delete;

    Signpost(Signpost&& other)
    {
        value = other.value;
        arg1 = other.arg1;
        arg2 = other.arg2;
        arg3 = other.arg3;
        color = other.color;
        other.value = 0;
    }

    explicit Signpost(unsigned short _value, SignpostColor _color = SignpostColor::Blue, unsigned short _arg1 = 0, unsigned short _arg2 = 0,
                      unsigned short _arg3 = 0)
        : arg1{_arg1}, arg2{_arg2}, arg3{_arg3}, color{_color}, value{static_cast<unsigned int>(_value)}
    {
#if !defined(NXA_SKIP_COCOA_PROFILING_SIGNPOSTS)
        if (value != 0) {
            kdebug_signpost_start(value, arg1, arg2, arg3, static_cast<unsigned long>(color));
        }
#endif
    }

    ~Signpost()
    {
#if !defined(NXA_SKIP_COCOA_PROFILING_SIGNPOSTS)
        if (value != 0) {
            kdebug_signpost_end(value, arg1, arg2, arg3, static_cast<unsigned long>(color));
        }
#endif
    }

private:
    unsigned int value;
};
}
#endif
