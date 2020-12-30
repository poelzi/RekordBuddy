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

#include <Base/Optional.hpp>
#include <Base/String.hpp>

#include <nlohmann/json.hpp>

namespace NxA {

// -- Public Interface
class JSon
{
    // -- Private Instance Variables
    nlohmann::json p_data;

public:
    // -- Constructors & Destructors
    JSon(const String& source)
    {
        try {
            this->p_data = nlohmann::json::parse(source.asUTF8());
        }
        catch(...) {
            // -- If anything happens, we just ignore it.
            this->p_data = nlohmann::json::array();
        }
    }

    // -- Instance Methods
    boolean contains(const String& key)
    {
        return this->p_data.contains(key.asUTF8());
    }
    String stringValueForKey(const String& key)
    {
        auto pos = this->p_data.find(key.asUTF8());
        NXA_ASSERT_TRUE(pos != this->p_data.end());

        return { pos->get<std::string>() };
    }
    Optional<String> maybeStringValueForKey(const String& key)
    {
        auto pos = this->p_data.find(key.asUTF8());
        if (pos == this->p_data.end()) {
            return { };
        }

        return { pos->get<std::string>() };
    }
};

}
