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

#include <AppleMusicCollection/Collection.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace AppleMusic {

// -- Constants

#define NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY           "UserITunesXMLFiles"

// -- Private Variables

static MutableArray<Common::CollectionOfSomeSort> collections;
static MutableArray<FilePath> collectionPaths;

// -- Functions

FilePath appleMusicXMLFilePath()
{
#if defined(NXA_PLATFORM_MACOS)
    return FilePath::filePathByJoiningPaths(Directory::userMusicDirectory(), NXA_FILEPATH("iTunes/iTunes Music Library.xml"));
#elif defined(NXA_PLATFORM_WINDOWS)
    return FilePath::filePathByJoiningPaths(Directory::userMusicDirectory(), NXA_FILEPATH("iTunes/iTunes Music Library.xml"));
#else
    #error Unsupported platform.
#endif

    NXA_ALOG("Not yet implemented.");
}

count approximateNumberOfAvailableAppleMusicCollections()
{
    count numberOfCollectionsFound = 0;

    MutableArray<String> sourceFilePaths;

    // -- Add the default rekordbox xml location
    if (File::existsAt(appleMusicXMLFilePath())) {
        ++numberOfCollectionsFound;
    }

    numberOfCollectionsFound += RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume())->arrayOfStringsForKey(String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY }).length();

    return numberOfCollectionsFound;
}

Array<Common::CollectionOfSomeSort> availableAppleMusicCollectionsWithPerCollectionProgressCallBack(const std::function<void(void)>& callback)
{
    MutableArray<Common::CollectionOfSomeSort> results;
    MutableArray<String> sourceFilePaths;

    NXA_BETA_LOG_WITH_FORMAT("User music folder is at '%s'.", Directory::userMusicDirectory().asFilePath().asEncodedString().asUTF8());
    NXA_BETA_LOG_WITH_FORMAT("Looking for itunes in User music folder is at '%s'.", appleMusicXMLFilePath().asEncodedString().asUTF8());

    // -- Add the default iTunes xml location
    auto appleMusicXMLBridgeFilePath = appleMusicXMLFilePath();
    sourceFilePaths.append(appleMusicXMLBridgeFilePath.asEncodedString());

    // -- Add any user-provided xml files
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());
    Common::Collection::makeSureStringsFromPreferenceKeyAreNotAlreadyInThenAddOnesThatAreNot(*userPreferences,
                                                                                             String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY },
                                                                                             sourceFilePaths);

    for (auto&& xmlFile : sourceFilePaths) {
        FilePath xmlFilePath{ xmlFile };

        auto maybeExisting = collectionPaths.find(xmlFilePath);
        if (maybeExisting != collectionPaths.end()) {
            results.append(collections[maybeExisting - collectionPaths.begin()]);
        }
        else if (File::existsAt(xmlFilePath)) {
            auto newCollection = Shared<Common::Collection>{ Shared<Collection>::with(xmlFilePath,
                                                                                      xmlFilePath != appleMusicXMLBridgeFilePath) };
            results.append(newCollection);

            collections.emplaceAppend(std::move(newCollection));
            collectionPaths.append(xmlFilePath);
        }

        callback();
    }

    return std::move(results);
}

boolean addUserCollectionAt(const FilePath& xmlFilePath)
{
    if (collectionPaths.contains(xmlFilePath)) {
        return false;
    }

    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    MutableArray<String> files{ userPreferences->arrayOfStringsForKey(String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY }) };

    auto fileAsString = xmlFilePath.asEncodedString();
    if (!files.contains(fileAsString)) {
        files.append(fileAsString);

        userPreferences->setArrayOfStringsForKey({ std::move(files) }, String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY });
    }

    return true;
}

boolean removeUserCollectionAt(const FilePath& xmlFilePath)
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    MutableArray<String> files{ userPreferences->arrayOfStringsForKey(String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY }) };

    auto fileAsString = xmlFilePath.asEncodedString();
    if (files.contains(fileAsString)) {
        files.remove(fileAsString);

        if (files.length() != 0) {
            userPreferences->setArrayOfStringsForKey({ std::move(files) }, String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY });
        }
        else {
            userPreferences->setArrayOfStringsForKey(nothing, String{ NXA_USER_ITUNES_XML_FILES_PREFERENCES_KEY });
        }
    }

    auto existingPosition = collectionPaths.find(xmlFilePath);
    NXA_ASSERT_TRUE(existingPosition != collectionPaths.end());

    auto existingIndex = existingPosition - collectionPaths.begin();
    collections.removeObjectAtIndex(existingIndex);
    collectionPaths.removeObjectAtIndex(existingIndex);

    return true;
}

} }
