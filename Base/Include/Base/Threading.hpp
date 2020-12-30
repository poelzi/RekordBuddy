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

#include <Base/String.hpp>
#include <Base/Types.hpp>

namespace NxA {

// -- Public Interface
class Threading final
{
public:
    // -- Types
    enum class Blocking {
        No,
        Yes
    };

    // -- Class Methods
    static void setIsOnMainThreadFunctionToUse(std::function<boolean()>&&);
    static void setRunOnMainThreadFunctionToUse(std::function<void(std::function<void()>&&, Blocking)>&&);
    static void setSleepInMillisecondsFunctionToUse(std::function<void(count)>&&);

    static boolean isOnMainThread();
    static void runOnMainThread(std::function<void()>&&, Blocking blocking);
    static void sleepInMilliseconds(count);

    // -- If you're sure all your code will run on the main thread, you can use this
    // -- to set all the methods above as pass-thru.
    static void setPassThruMethodsForAll();
};

}
