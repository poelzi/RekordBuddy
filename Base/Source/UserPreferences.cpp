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

#include <Base/Blob.hpp>
#include <Base/File.hpp>
#include <Base/FilePath.hpp>
#include <Base/List.hpp>
#include <Base/UserPreferences.hpp>

using namespace NxA;

// -- Class Variables

// -- These preferences are shared and could be accessed by different threads so we need to be thread safe at least
// -- when trying to access or create new ones.
std::mutex UserPreferences::p_globalMutex;

// -- We return NotNull<UserPreferences*> to users of user preferences so we need to make sure they don't move
// -- around or get re-allocated when new preferences are added. That's why we keep both a list and a map.
MutableList<UserPreferences> UserPreferences::p_allUserPreferences;
MutableMap<FilePath, NotNull<UserPreferences*>> UserPreferences::p_userDefaultsPerFilePath;

Optional<std::function<void(NotNull<UserPreferences*>)>> UserPreferences::p_maybeDefaultsSettingsCallBack;

// -- Class Methods

void UserPreferences::setDefaultValuesWith(std::function<void(NotNull<UserPreferences*>)>&& callback)
{
    UserPreferences::p_globalMutex.lock();

    UserPreferences::p_maybeDefaultsSettingsCallBack = { std::move(callback) };

    UserPreferences::p_globalMutex.unlock();
}

NotNull<UserPreferences*> UserPreferences::fromFile(const FilePath& path)
{
    UserPreferences::p_globalMutex.lock();

    auto maybeDefaults = UserPreferences::p_userDefaultsPerFilePath.maybeValueForKey(path);
    if (!maybeDefaults.isValid()) {
        UserPreferences::p_allUserPreferences.emplaceAppend();
        auto newPreferences = NotNull<UserPreferences*>{ &UserPreferences::p_allUserPreferences.lastObject() };

        if (File::existsAt(path)) {
            // -- If we have a preference file on disc, we load it.
            newPreferences->p_loadFromFile(path);
        }
        else if (UserPreferences::p_maybeDefaultsSettingsCallBack.isValid()) {
            // -- Otherwise we set defaults if someone told us we had any to set.
            (*UserPreferences::p_maybeDefaultsSettingsCallBack)(newPreferences);
        }

        UserPreferences::p_userDefaultsPerFilePath.setValueForKey(newPreferences, path);

        maybeDefaults = newPreferences;
    }

    UserPreferences::p_globalMutex.unlock();

    return *maybeDefaults;
}

// -- Instance Methods

boolean UserPreferences::p_loadFromFile(const FilePath& path)
{
    this->p_mutex.lock();

    auto maybeContent = File::maybeContentOfFileAt(path);
    if (!maybeContent.isValid()) {
        this->p_mutex.unlock();
        return false;
    }

    auto contentAsStrings = maybeContent->asStringsFromZeroSeparatedData();
    count currentStringIndex = 0;
    count numberOfStrings = contentAsStrings.length();

    while ((currentStringIndex + 3) <= numberOfStrings) {
        auto& key = contentAsStrings[currentStringIndex++];
        if (key.length() == 0) {
            // -- Something got corrupted, we should not have zero length keys.
            this->p_mutex.unlock();
            return false;
        }

        auto& marker = contentAsStrings[currentStringIndex++];
        if (marker.length() != 1) {
            // -- Something got corrupted, we should not have markers with more than one characters.
            this->p_mutex.unlock();
            return false;
        }

        switch (marker[0]) {
            case UserPreferences::p_booleanEntryMarker: {
                this->p_booleanUserPreferences.setValueForKey((contentAsStrings[currentStringIndex++][0] == '1'), key);
                break;
            }
            case UserPreferences::p_integerEntryMarker: {
                this->p_integerUserPreferences.setValueForKey(contentAsStrings[currentStringIndex++].integerValue(), key);
                break;
            }
            case UserPreferences::p_stringEntryMarker: {
                this->p_stringUserPreferences.setValueForKey(contentAsStrings[currentStringIndex++], key);
                break;
            }
            case UserPreferences::p_stringArrayEntryMarker: {
                if ((currentStringIndex + 2) >= numberOfStrings) {
                    // -- Something got corrupted, we should have at least 4 strings total here.
                    this->p_mutex.unlock();
                    return false;
                }

                count length = contentAsStrings[currentStringIndex++].integerValue();
                if ((length == 0) || (currentStringIndex + length) > numberOfStrings) {
                    // -- Something got corrupted, we shouldn't have empty arrays in the file or lengths that go past the end of our data.
                    this->p_mutex.unlock();
                    return false;
                }

                MutableArray<String> arrayContent;
                for (count index = 0; index < length; ++index) {
                    if (currentStringIndex >= numberOfStrings) {
                        // -- Something got corrupted, we shouldn't have strings that go past the end of our data.
                        this->p_mutex.unlock();
                        return false;
                    }

                    arrayContent.emplaceAppend(std::move(contentAsStrings[currentStringIndex++]));
                }

                this->p_stringArrayUserPreferences.setValueForKey(std::move(arrayContent), key);
                break;
            }
            case UserPreferences::p_integerArrayEntryMarker: {
                if ((currentStringIndex + 2) >= numberOfStrings) {
                    // -- Something got corrupted, we should have at least 4 strings total here.
                    this->p_mutex.unlock();
                    return false;
                }

                count length = contentAsStrings[currentStringIndex++].integerValue();
                if ((length == 0) || (currentStringIndex + length) > numberOfStrings) {
                    // -- Something got corrupted, we shouldn't have empty arrays in the file or lengths that go past the end of our data.
                    this->p_mutex.unlock();
                    return false;
                }

                MutableArray<integer> arrayContent;
                for (count index = 0; index < length; ++index) {
                    if (currentStringIndex >= numberOfStrings) {
                        // -- Something got corrupted, we shouldn't have strings that go past the end of our data.
                        this->p_mutex.unlock();
                        return false;
                    }

                    arrayContent.emplaceAppend(contentAsStrings[currentStringIndex++].integerValue());
                }

                this->p_integerArrayUserPreferences.setValueForKey(std::move(arrayContent), key);
                break;
            }
            case UserPreferences::p_blobEntryMarker: {
                this->p_blobUserPreferences.setValueForKey(Blob::withBase64String(contentAsStrings[currentStringIndex++]), key);
                break;
            }
            default: {
                // -- Something got corrupted and we can't find valid entries in the file.
                this->p_mutex.unlock();
                return false;
            }
        }
    }

    this->p_userPreferencesDirty = false;

    this->p_mutex.unlock();

    return true;
}

boolean UserPreferences::saveToFile(const FilePath& path) const
{
    this->p_mutex.lock();

    if (this->p_userPreferencesDirty) {
        MutableBlob resultingData;

        for (auto&& booleanEntry : this->p_booleanUserPreferences) {
            resultingData.appendWithStringTermination(booleanEntry.first.asUTF8());
            resultingData.append(UserPreferences::p_booleanEntryMarker);
            resultingData.append(0);
            resultingData.appendWithStringTermination(booleanEntry.second ? "1" : "0");
        }

        for (auto&& integerEntry : this->p_integerUserPreferences) {
            resultingData.appendWithStringTermination(integerEntry.first.asUTF8());
            resultingData.append(UserPreferences::p_integerEntryMarker);
            resultingData.append(0);
            resultingData.appendWithStringTermination(String::stringWithFormat("%ld", integerEntry.second).asUTF8());
        }

        for (auto&& stringEntry : this->p_stringUserPreferences) {
            resultingData.appendWithStringTermination(stringEntry.first.asUTF8());
            resultingData.append(UserPreferences::p_stringEntryMarker);
            resultingData.append(0);
            resultingData.appendWithStringTermination(stringEntry.second.asUTF8());
        }

        for (auto&& stringArrayEntry : this->p_stringArrayUserPreferences) {
            resultingData.appendWithStringTermination(stringArrayEntry.first.asUTF8());
            resultingData.append(UserPreferences::p_stringArrayEntryMarker);
            resultingData.append(0);

            auto& arrayOfStrings = stringArrayEntry.second;
            if (!arrayOfStrings.length()) {
                // -- We don't store empty arrays.
                continue;
            }
            resultingData.appendWithStringTermination(String::stringWithFormat("%ld", arrayOfStrings.length()).asUTF8());

            for (auto&& stringValue : arrayOfStrings) {
                resultingData.appendWithStringTermination(stringValue.asUTF8());
            }
        }

        for (auto&& integerArrayEntry : this->p_integerArrayUserPreferences) {
            resultingData.appendWithStringTermination(integerArrayEntry.first.asUTF8());
            resultingData.append(UserPreferences::p_integerArrayEntryMarker);
            resultingData.append(0);

            auto& arrayOfIntegers = integerArrayEntry.second;
            if (!arrayOfIntegers.length()) {
                // -- We don't store empty arrays.
                continue;
            }
            resultingData.appendWithStringTermination(String::stringWithFormat("%ld", arrayOfIntegers.length()).asUTF8());

            for (auto&& integerValue : arrayOfIntegers) {
                resultingData.appendWithStringTermination(String::stringWithFormat("%ld", integerValue).asUTF8());
            }
        }

        for (auto&& blobEntry : this->p_blobUserPreferences) {
            resultingData.appendWithStringTermination(blobEntry.first.asUTF8());
            resultingData.append(UserPreferences::p_blobEntryMarker);
            resultingData.append(0);
            resultingData.appendWithStringTermination(blobEntry.second.base64String().asUTF8());
        }

        File::writeBlobToFileAt(resultingData, path);

        this->p_userPreferencesDirty = false;
    }

    this->p_mutex.unlock();

    return true;
}

void UserPreferences::clearAll()
{
    this->p_mutex.lock();

    this->p_booleanUserPreferences.removeAll();
    this->p_integerUserPreferences.removeAll();
    this->p_stringUserPreferences.removeAll();
    this->p_stringArrayUserPreferences.removeAll();
    this->p_integerArrayUserPreferences.removeAll();
    this->p_blobUserPreferences.removeAll();

    this->p_userPreferencesDirty = false;

    this->p_mutex.unlock();
}

void UserPreferences::setDefaultBooleanForKey(boolean value, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (!this->p_booleanUserPreferences.hasValueForKey(key)) {
        this->p_booleanUserPreferences.setValueForKey(value, key);

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

Optional<boolean> UserPreferences::maybeBooleanForKey(const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return nothing;
    }

    this->p_mutex.lock();

    auto value = this->p_booleanUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    return value;
}

void UserPreferences::setBooleanForKey(Optional<boolean> maybeValue, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (this->p_booleanUserPreferences.maybeValueForKey(key) != maybeValue) {
        if (maybeValue.isValid()) {
            this->p_booleanUserPreferences.setValueForKey(*maybeValue, key);
        }
        else {
            this->p_booleanUserPreferences.removeValueForKey(key);
        }

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

void UserPreferences::setDefaultIntegerForKey(integer value, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (!this->p_integerUserPreferences.hasValueForKey(key)) {
        this->p_integerUserPreferences.setValueForKey(value, key);

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

Optional<integer> UserPreferences::maybeIntegerForKey(const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return nothing;
    }

    this->p_mutex.lock();

    auto value = p_integerUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    return value;
}

void UserPreferences::setIntegerForKey(Optional<integer> maybeValue, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (this->p_integerUserPreferences.maybeValueForKey(key) != maybeValue) {
        if (maybeValue.isValid()) {
            this->p_integerUserPreferences.setValueForKey(*maybeValue, key);
        }
        else {
            this->p_integerUserPreferences.removeValueForKey(key);
        }

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

void UserPreferences::setDefaultStringForKey(const String& value, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (!this->p_stringUserPreferences.hasValueForKey(key)) {
        this->p_stringUserPreferences.setValueForKey(value, key);

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

Optional<String> UserPreferences::maybeStringForKey(const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return nothing;
    }

    this->p_mutex.lock();

    auto value = this->p_stringUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    return value;
}

void UserPreferences::setStringForKey(Optional<String> maybeValue, const String& key)
{
    auto valueIsValid = maybeValue.isValid();

    if ((key.length() == 0) || (valueIsValid && (maybeValue->length() == 0))) {
        // -- We don't support adding zero length strings or zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (this->p_stringUserPreferences.maybeValueForKey(key) != maybeValue) {
        if (valueIsValid) {
            this->p_stringUserPreferences.setValueForKey(*maybeValue, key);
        }
        else {
            this->p_stringUserPreferences.removeValueForKey(key);
        }

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

void UserPreferences::setDefaultArrayOfStringsForKey(const Array<String>& value, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (!this->p_stringArrayUserPreferences.hasValueForKey(key)) {
        this->p_stringArrayUserPreferences.setValueForKey(value, key);

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

Array<String> UserPreferences::arrayOfStringsForKey(const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return { };
    }

    this->p_mutex.lock();

    auto maybeValue = this->p_stringArrayUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    return maybeValue.isValid() ? *maybeValue : Array<String>{ };
}

Optional<String> UserPreferences::maybeStringAtIndexForKey(count index, const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return nothing;
    }

    this->p_mutex.lock();

    auto maybeValue = this->p_stringArrayUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    if (!maybeValue.isValid() || (index >= maybeValue->length())) {
        return nothing;
    }

    return (*maybeValue)[index];
}

void UserPreferences::setArrayOfStringsForKey(Optional<Array<String>> maybeValues, const String& key)
{
    auto valuesAreValid = maybeValues.isValid();

    if ((key.length() == 0) || (valuesAreValid && (maybeValues->length() == 0))) {
        // -- We don't support adding empty arrays or zero length keys.
        return;
    }

    if (valuesAreValid) {
        for (auto&& stringValue : *maybeValues) {
            if (stringValue.length() == 0) {
                // -- We don't support adding zero length strings.
                return;
            }
        }
    }

    this->p_mutex.lock();

    if (valuesAreValid) {
        this->p_stringArrayUserPreferences.setValueForKey(*maybeValues, key);
    }
    else {
        this->p_stringArrayUserPreferences.removeValueForKey(key);
    }

    this->p_userPreferencesDirty = true;

    this->p_mutex.unlock();
}

void UserPreferences::setDefaultArrayOfIntegersForKey(const Array<integer>& value, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (!this->p_integerArrayUserPreferences.hasValueForKey(key)) {
        this->p_integerArrayUserPreferences.setValueForKey(value, key);

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

Array<integer> UserPreferences::arrayOfIntegersForKey(const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return { };
    }

    this->p_mutex.lock();

    auto maybeValue = this->p_integerArrayUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    return maybeValue.isValid() ? *maybeValue : Array<integer>{ };
}

Optional<integer> UserPreferences::maybeIntegerAtIndexForKey(count index, const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return nothing;
    }

    this->p_mutex.lock();

    auto maybeValue = this->p_integerArrayUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    if (!maybeValue.isValid() || (index >= maybeValue->length())) {
        return nothing;
    }

    return (*maybeValue)[index];
}

void UserPreferences::setArrayOfIntegersForKey(Optional<Array<integer>> maybeValues, const String& key)
{
    auto valuesAreValid = maybeValues.isValid();

    if ((key.length() == 0) || (valuesAreValid && (maybeValues->length() == 0))) {
        // -- We don't support adding empty arrays or zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (valuesAreValid) {
        this->p_integerArrayUserPreferences.setValueForKey(*maybeValues, key);
    }
    else {
        this->p_integerArrayUserPreferences.removeValueForKey(key);
    }

    this->p_userPreferencesDirty = true;

    this->p_mutex.unlock();
}

void UserPreferences::setDefaultBlobForKey(const Blob& value, const String& key)
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (!this->p_blobUserPreferences.hasValueForKey(key)) {
        this->p_blobUserPreferences.setValueForKey(value, key);

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}

Optional<Blob> UserPreferences::maybeBlobForKey(const String& key) const
{
    if (key.length() == 0) {
        // -- We don't support zero length keys.
        return nothing;
    }

    this->p_mutex.lock();

    auto value = this->p_blobUserPreferences.maybeValueForKey(key);

    this->p_mutex.unlock();

    return value;
}

void UserPreferences::setBlobForKey(Optional<Blob> maybeValue, const String& key)
{
    auto valueIsValid = maybeValue.isValid();

    if ((key.length() == 0) || (valueIsValid && (maybeValue->size() == 0))) {
        // -- We don't support adding zero length blobs or zero length keys.
        return;
    }

    this->p_mutex.lock();

    if (this->p_blobUserPreferences.maybeValueForKey(key) != maybeValue) {
        if (valueIsValid) {
            this->p_blobUserPreferences.setValueForKey(*maybeValue, key);
        }
        else {
            this->p_blobUserPreferences.removeValueForKey(key);
        }

        this->p_userPreferencesDirty = true;
    }

    this->p_mutex.unlock();
}
