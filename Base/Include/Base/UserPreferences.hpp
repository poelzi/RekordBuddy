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

#include <Base/Blob.hpp>
#include <Base/List.hpp>
#include <Base/Map.hpp>
#include <Base/Optional.hpp>
#include <Base/String.hpp>
#include <Base/Types.hpp>

#include <mutex>

namespace NxA {

// -- Forward Declarations
struct FilePath;

// -- Public Interface
class UserPreferences
{
    // -- Friends
    friend class UserPreferencesTests;

    // -- Private Class Variables
    static Optional<std::function<void(NotNull<UserPreferences*>)>> p_maybeDefaultsSettingsCallBack;

    static constexpr character p_booleanEntryMarker         = 'B';
    static constexpr character p_integerEntryMarker         = 'I';
    static constexpr character p_stringEntryMarker          = 'S';
    static constexpr character p_stringArrayEntryMarker     = 'A';
    static constexpr character p_integerArrayEntryMarker    = 'Y';
    static constexpr character p_blobEntryMarker            = 'X';

    // -- These preferences are shared and could be accessed by different threads so we need to be thread safe at least
    // -- when trying to access or create new ones.
    static std::mutex p_globalMutex;

    // -- We return NotNull<UserPreferences*> to users of user preferences so we need to make sure they don't move
    // -- around or get re-allocated when new preferences are added. That's why we keep both a list and a map.
    static MutableList<UserPreferences> p_allUserPreferences;
    static MutableMap<FilePath, NotNull<UserPreferences*>> p_userDefaultsPerFilePath;

    // -- Private Instance Variables
    MutableMap<String, boolean> p_booleanUserPreferences;
    MutableMap<String, integer> p_integerUserPreferences;
    MutableMap<String, String> p_stringUserPreferences;
    MutableMap<String, Array<String>> p_stringArrayUserPreferences;
    MutableMap<String, Array<integer>> p_integerArrayUserPreferences;
    MutableMap<String, Blob> p_blobUserPreferences;

    mutable boolean p_userPreferencesDirty = false;

    // -- These preferences are shared and could be accessed by different threads so we need to be thread safe.
    mutable std::recursive_mutex p_mutex;

protected:
    // -- Protected Instance Methods
    boolean p_loadFromFile(const FilePath&);

public:
    // -- Class Methods
    static void setDefaultValuesWith(std::function<void(NotNull<UserPreferences*>)>&&);

    static NotNull<UserPreferences*> fromFile(const FilePath&);

    static Optional<boolean> maybeOSBooleanForKey(const String&);
    static Optional<integer> maybeOSIntegerForKey(const String&);
    static Optional<String> maybeOSStringForKey(const String&);

    // -- Instance methods
    boolean saveToFile(const FilePath&) const;
    void clearAll();

    void setDefaultBooleanForKey(boolean, const String&);
    Optional<boolean> maybeBooleanForKey(const String&) const;
    void setBooleanForKey(Optional<boolean>, const String&);

    void setDefaultIntegerForKey(integer, const String&);
    Optional<integer> maybeIntegerForKey(const String&) const;
    void setIntegerForKey(Optional<integer>, const String&);

    void setDefaultStringForKey(const String&, const String&);
    Optional<String> maybeStringForKey(const String&) const;
    void setStringForKey(Optional<String>, const String&);

    void setDefaultArrayOfStringsForKey(const Array<String>&, const String&);
    Array<String> arrayOfStringsForKey(const String&) const;
    Optional<String> maybeStringAtIndexForKey(count, const String&) const;
    void setArrayOfStringsForKey(Optional<Array<String>>, const String&);

    void setDefaultArrayOfIntegersForKey(const Array<integer>&, const String&);
    Array<integer> arrayOfIntegersForKey(const String&) const;
    Optional<integer> maybeIntegerAtIndexForKey(count, const String&) const;
    void setArrayOfIntegersForKey(Optional<Array<integer>>, const String&);

    void setDefaultBlobForKey(const Blob&, const String&);
    Optional<Blob> maybeBlobForKey(const String&) const;
    void setBlobForKey(Optional<Blob>, const String&);
};

}
