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

#include <RekordboxCollection/Collection.hpp>
#include <RekordboxCollection/Rekordbox.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace Rekordbox {

// -- Constants

#define NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY           "UserRekordBoxXMLFiles"

// -- Private Variables

static String p_rekordBuddyVersionString{ "Unknown" };
static MutableArray<Common::CollectionOfSomeSort> collections;
static MutableArray<FilePath> collectionPaths;

// -- Functions

static Optional<FilePath> p_maybeRekordboxXMLBridgeFilePathIfRekordboxInstalled()
{
    if (!Rekordbox::rekordboxXMLBridgeDirectoryPath().exists()) {
        // -- rekordbox is maybe no installed.
        return nothing;
    }

    auto bridgeFilePath = Rekordbox::rekordboxXMLBridgeFilePath();
    if (!File::existsAt(bridgeFilePath)) {
        File::writeStringToFileAt(String::stringWithFormat("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
                                                           "<DJ_PLAYLISTS Version=\"1.0.0\">\n"
                                                           "    <PRODUCT Name=\"Rekord Buddy\" Version=\"%s\" Company=\"Didier Malenfant\"></PRODUCT>\n"
                                                           "    <COLLECTION Entries=\"0\">\n"
                                                           "    </COLLECTION>\n"
                                                           "    <PLAYLISTS>\n"
                                                           "        <NODE Type=\"0\" Name=\"ROOT\" Count=\"0\">\n"
                                                           "        </NODE>\n"
                                                           "    </PLAYLISTS>\n"
                                                           "</DJ_PLAYLISTS>", p_rekordBuddyVersionString.asUTF8()),
                                                           bridgeFilePath);
    }

    return bridgeFilePath;
}

void setCurrentRekordBuddyVersion(const String& versionString)
{
    p_rekordBuddyVersionString = versionString;
}

FilePath rekordboxXMLBridgeFilePath()
{
    return FilePath::filePathByJoiningPaths(Rekordbox::rekordboxXMLBridgeDirectoryPath(), NXA_FILEPATH("rekordbox.xml"));
}

Directory rekordboxXMLBridgeDirectoryPath()
{
#if defined(NXA_PLATFORM_MACOS)
    return Directory{ FilePath::filePathByJoiningPaths(Directory::userLibraryDirectory(), NXA_FILEPATH("Pioneer/rekordbox")) };
#elif defined(NXA_PLATFORM_WINDOWS)
    return Directory{ FilePath::filePathByJoiningPaths(Directory::userRoamingAppDataDirectory(), NXA_FILEPATH("Pioneer/rekordbox")) };
#else
    #error Unsupported platform.
#endif
}

count approximateNumberOfAvailableRekordboxCollections()
{
    count numberOfCollectionsFound = 0;

    if (p_maybeRekordboxXMLBridgeFilePathIfRekordboxInstalled().isValid()) {
        ++numberOfCollectionsFound;
    }

    numberOfCollectionsFound += RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume())->arrayOfStringsForKey(String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY }).length();

    return numberOfCollectionsFound;
}

Array<Common::CollectionOfSomeSort> availableRekordboxCollectionsWithPerCollectionProgressCallBack(const std::function<void(void)>& callback)
{
    MutableArray<Common::CollectionOfSomeSort> results;
    MutableArray<String> sourceFilePaths;

    // -- Add the default rekordbox xml location if rekorbox is installed
    auto maybeRekordboxXMLBridgeFilePath = p_maybeRekordboxXMLBridgeFilePathIfRekordboxInstalled();
    if (maybeRekordboxXMLBridgeFilePath.isValid()) {
        sourceFilePaths.append(maybeRekordboxXMLBridgeFilePath->asEncodedString());
    }

    // -- Add any user-provided xml files
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());
    Common::Collection::makeSureStringsFromPreferenceKeyAreNotAlreadyInThenAddOnesThatAreNot(*userPreferences,
                                                                                             String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY },
                                                                                             sourceFilePaths);

    for (auto&& xmlFile : sourceFilePaths) {
        FilePath xmlFilePath{ xmlFile };

        auto maybeExisting = collectionPaths.find(xmlFilePath);
        if (maybeExisting != collectionPaths.end()) {
            results.append(collections[maybeExisting - collectionPaths.begin()]);
        }
        else if (File::existsAt(xmlFilePath)) {
            auto newCollection = Shared<Common::MutableCollection>{ Shared<MutableCollection>::with(xmlFilePath,
                                                                                                    maybeRekordboxXMLBridgeFilePath.isValid() &&
                                                                                                    (xmlFilePath != *maybeRekordboxXMLBridgeFilePath)) };
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

    MutableArray<String> files{ userPreferences->arrayOfStringsForKey(String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY }) };

    auto fileAsString = xmlFilePath.asEncodedString();
    if (!files.contains(fileAsString)) {
        files.append(fileAsString);

        userPreferences->setArrayOfStringsForKey({ std::move(files) }, String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY });
    }

    return true;
}

boolean removeUserCollectionAt(const FilePath& xmlFilePath)
{
    auto userPreferences = RekordBuddy::UserPreferences::forVolume(Volume::musicFolderVolume());

    MutableArray<String> files{ userPreferences->arrayOfStringsForKey(String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY }) };

    auto fileAsString = xmlFilePath.asEncodedString();
    if (files.contains(fileAsString)) {
        files.remove(fileAsString);

        if (files.length() != 0) {
            userPreferences->setArrayOfStringsForKey({ std::move(files) }, String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY });
        }
        else {
            userPreferences->setArrayOfStringsForKey(nothing, String{ NXA_USER_REKORDBOX_XML_FILES_PREFERENCES_KEY });
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
