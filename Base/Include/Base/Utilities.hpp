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

#include <Base/Types.hpp>
#include <Base/Assert.hpp>
#include <Base/Optional.hpp>

#include <limits>

namespace NxA {

// -- Checked numeric narrow-casting. If the narrow-cast loses bits, we return nothing. usage: maybeNarrowCast<integer64>(someUnsignedInteger)
template<class Target, class Source>
    Optional<Target> maybeNarrowCast(Source v)
    {
        static_assert(!(double(std::numeric_limits<Target>::max()) > double(std::numeric_limits<Source>::max()) &&
                        double(std::numeric_limits<Target>::min()) < double(std::numeric_limits<Source>::min())),
                                "maybeNarrowCast used where unneeded; Target contains a value for every source value");
        auto r = static_cast<Target>(v);
        if (static_cast<Source>(r) != v) {
            return nothing;
        }
        return r;
    }

}
