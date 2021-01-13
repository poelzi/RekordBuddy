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

#include <PCDJCollection/Collection.hpp>
#include <PCDJCollection/PCDJ.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/Base.hpp>

namespace NxA { namespace PCDJ {

// -- Private Variables

static MutableArray<Common::CollectionOfSomeSort> collections;
static MutableArray<FilePath> collectionPaths;

// -- Functions

static Optional<FilePath> p_maybePCDJXMLDatabaseFilePathIfPCDJInstalled()
{
#if defined(NXA_PLATFORM_MACOS)
    auto databaseDirectory = Directory{ FilePath::filePathByJoiningPaths(Directory::applicationSupportDirectory(), NXA_FILEPATH("PCDJ-DEX3"), NXA_FILEPATH("Database")) };
    if (!databaseDirectory.exists()) {
        databaseDirectory = Directory{ FilePath::filePathByJoiningPaths(Directory::userDocumentsDirectory(), NXA_FILEPATH("PCDJ-DEX3"), NXA_FILEPATH("Database")) };
    }
#elif defined(NXA_PLATFORM_WINDOWS)
    auto databaseDirectory = Directory{ FilePath::filePathByJoiningPaths(Directory::userLocalAppDataDirectory(), NXA_FILEPATH("PCDJ-DEX3"), NXA_FILEPATH("Database")) };
    if (!databaseDirectory.exists()) {
        databaseDirectory = Directory{ FilePath::filePathByJoiningPaths(Directory::userDocumentsDirectory(), NXA_FILEPATH("PCDJ-DEX3"), NXA_FILEPATH("Database")) };
    }
#else
    #error Unsupported platform.
#endif

    if (databaseDirectory.exists()) {
        auto databaseFilePath = FilePath::filePathByJoiningPaths(databaseDirectory, NXA_FILEPATH("database.xml"));
        if (File::existsAt(databaseFilePath)) {
            return databaseFilePath;
        }
    }

    // -- PCDJ is maybe not installed.
    return nothing;
}

count approximateNumberOfAvailablePCDJCollections()
{
    return p_maybePCDJXMLDatabaseFilePathIfPCDJInstalled().isValid() ? 1 : 0;
}

Array<Common::CollectionOfSomeSort> availablePCDJCollectionsWithPerCollectionProgressCallBack(const std::function<void(void)>& callback)
{
    // -- Add the default PCDJ xml location if PCDJ is installed
    auto maybePCDJXMLDatabaseFilePath = p_maybePCDJXMLDatabaseFilePathIfPCDJInstalled();
    if (!maybePCDJXMLDatabaseFilePath.isValid()) {
        return { };
    }

    MutableArray<Common::CollectionOfSomeSort> results;
    results.append(Shared<Common::Collection>{ Shared<MutableCollection>::with(*maybePCDJXMLDatabaseFilePath) });

    callback();

    return std::move(results);
}

} }
