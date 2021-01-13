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

#include "TraktorCollection/Collection.hpp"

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Traktor {

// -- Constants

#define NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY           "UserTraktorNMLFiles"

// -- Private Variables

static MutableArray<Common::CollectionOfSomeSort> collections;
static MutableArray<FilePath> collectionPaths;

// -- Functions

count approximateNumberOfAvailableTraktorCollections()
{
    count numberOfCollectionsFound = 0;

    auto traktorDirectory = Directory{ FilePath::filePathByJoiningPaths(Directory::userDocumentsDirectory(),
                                                                        NXA_FILEPATH("Native Instruments")) };
    if (traktorDirectory.exists()) {
        for (auto&& directoryPath : traktorDirectory.directoriesInDirectory()) {
            auto components = directoryPath.asFilePath().componentsOfPath();
            auto directoryName = components.lastObject().asEncodedString();

            if (directoryName.hasPrefix("Traktor ")) {
                ++numberOfCollectionsFound;
            }
        }
    }

    numberOfCollectionsFound += RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume())->arrayOfStringsForKey(String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY }).length();

    return numberOfCollectionsFound;
}

Array<Common::CollectionOfSomeSort> availableTraktorCollectionsWithPerCollectionProgressCallBack(const std::function<void(void)>& callback)
{
    MutableArray<Common::CollectionOfSomeSort> results;
    MutableArray<String> sourceFilePaths;
    MutableArray<String> versionNumbers;

    auto traktorDirectory = Directory{ FilePath::filePathByJoiningPaths(Directory::userDocumentsDirectory(),
                                                                        NXA_FILEPATH("Native Instruments")) };
    if (traktorDirectory.exists()) {
        for (auto&& directoryPath : traktorDirectory.directoriesInDirectory()) {
            auto components = directoryPath.asFilePath().componentsOfPath();
            auto directoryName = components.lastObject().asEncodedString();

            if (!directoryName.hasPrefix("Traktor ")) {
                continue;
            }

            auto collectionFilePath = FilePath::filePathByJoiningPaths(directoryPath, NXA_FILEPATH("collection.nml"));
            if (!File::existsAt(collectionFilePath)) {
                continue;
            }

            auto versionNumber = directoryName.subString(8);
            bool validDirectory = true;
            for (count index = 0; index < versionNumber.length(); ++index) {
                auto character = versionNumber[index];
                if ((character != '.') && ((character < '0') || (character > '9'))) {
                    validDirectory = false;
                }
            }

            if (!validDirectory) {
                continue;
            }

            versionNumbers.append(versionNumber);
            sourceFilePaths.append(collectionFilePath.asEncodedString());
        }

        // -- TODO: This should be replaced by an equivalent method to localizedStandardCompare: in ObjC.
        sourceFilePaths.sort();
        versionNumbers.sort();
    }

    // -- Add any user-provided nml files
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());
    Common::Collection::makeSureStringsFromPreferenceKeyAreNotAlreadyInThenAddOnesThatAreNot(*userPreferences,
                                                                                             String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY },
                                                                                             sourceFilePaths);

    count currentCollectionIndex = 0;
    for (auto&& nmlFile : sourceFilePaths) {
        FilePath nmlFilePath{ nmlFile };

        auto maybeExisting = collectionPaths.find(nmlFilePath);
        if (maybeExisting != collectionPaths.end()) {
            results.append(collections[maybeExisting - collectionPaths.begin()]);
        }
        else if (File::existsAt(nmlFilePath)) {
            auto collectionName = "Traktor"_String;
            auto isAnInstalledVersionOfTraktor = currentCollectionIndex < versionNumbers.length();
            if (isAnInstalledVersionOfTraktor) {
                collectionName = collectionName.stringByAppending(" ").stringByAppending(versionNumbers[currentCollectionIndex]);
            }

            auto newCollection = Shared<Common::MutableCollection>{ Shared<MutableCollection>::with(nmlFilePath,
                                                                                                    collectionName,
                                                                                                    !isAnInstalledVersionOfTraktor) };
            results.append(newCollection);

            collections.emplaceAppend(std::move(newCollection));
            collectionPaths.append(nmlFilePath);
        }

        ++currentCollectionIndex;

        callback();
    }

    return std::move(results);
}

boolean addUserCollectionAt(const FilePath& nmlFilePath)
{
    if (collectionPaths.contains(nmlFilePath)) {
        return false;
    }

    auto userDefaults = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    MutableArray<String> files = userDefaults->arrayOfStringsForKey(String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY });

    auto fileAsString = nmlFilePath.asEncodedString();
    if (!files.contains(fileAsString)) {
        files.append(fileAsString);

        userDefaults->setArrayOfStringsForKey({ std::move(files) }, String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY });
    }

    return true;
}

boolean removeUserCollectionAt(const FilePath& nmlFilePath)
{
    auto userDefaults = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    MutableArray<String> files = userDefaults->arrayOfStringsForKey(String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY });

    auto fileAsString = nmlFilePath.asEncodedString();
    if (files.contains(fileAsString)) {
        files.remove(fileAsString);

        if (files.length() != 0) {
            userDefaults->setArrayOfStringsForKey({ std::move(files) }, String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY });
        }
        else {
            userDefaults->setArrayOfStringsForKey(nothing, String{ NXA_USER_TRAKTOR_NML_FILES_PREFERENCES_KEY });
        }
    }

    auto existingPosition = collectionPaths.find(nmlFilePath);
    NXA_ASSERT_TRUE(existingPosition != collectionPaths.end());

    auto existingIndex = existingPosition - collectionPaths.begin();
    collections.removeObjectAtIndex(existingIndex);
    collectionPaths.removeObjectAtIndex(existingIndex);

    return true;
}

} }
