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

#include <Base/Map.hpp>
#include <Base/String.hpp>

namespace NxA {

// -- Public Interface
class EventLog
{
public:
    // -- Constants
    enum class Asynchronous
    {
        No,
        Yes
    };

private:
    // -- Private Class Variables
    static Optional<String> p_maybeToken;
    static Optional<String> p_maybeUserID;

    // -- Private Class Methods
    static Optional<String> p_maybeStringWithContentsOfURL(const String&);
    static void p_requestForURL(const String&, Asynchronous);

public:
    // -- Constants
    enum class SessionEndedNormally {
        No,
        Yes
    };

    // -- Class Methods
    static Optional<String> maybeNewRandomUserID();

    static void initWithTokenUserID(const String&, const String&);

    static void sessionStarted(const Optional<Map<String, boolean>>& = nothing);
    static void sessionEnded(SessionEndedNormally = SessionEndedNormally::Yes);

    static void setUserInfo(const String&, const Optional<String>&);
    static void setUserInfo(const String&, const Array<String>&);

    static void event(const String&, Asynchronous = Asynchronous::Yes);
    static void event(const String&, const Map<String, String>&, Asynchronous = Asynchronous::Yes);
    static void event(const String&, const Map<String, boolean>&, Asynchronous = Asynchronous::Yes);
};

}
