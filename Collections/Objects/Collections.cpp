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

#include <Collections/Collections.hpp>

#include <RekordBuddyCollection/RekordBuddy.hpp>
#include <RekordboxCollection/Rekordbox.hpp>
#include <SeratoCollection/Serato.hpp>
#include <TraktorCollection/Traktor.hpp>
#include <AppleMusicCollection/AppleMusic.hpp>
#include <PCDJCollection/PCDJ.hpp>

#include <RekordBuddyCollection/UserPreferences.hpp>

#include <Base/Variant.hpp>

namespace NxA { namespace Common {

// -- Functions

count approximateNumberOfAvailableCollections()
{
    count numberOfCollectionsFound = 0;

    numberOfCollectionsFound += RekordBuddy::approximateNumberOfAvailableRekordBuddyCollections();
    numberOfCollectionsFound += Rekordbox::approximateNumberOfAvailableRekordboxCollections();
    numberOfCollectionsFound += Serato::approximateNumberOfAvailableSeratoCollections();
    numberOfCollectionsFound += Traktor::approximateNumberOfAvailableTraktorCollections();
    numberOfCollectionsFound += PCDJ::approximateNumberOfAvailablePCDJCollections();
    numberOfCollectionsFound += AppleMusic::approximateNumberOfAvailableAppleMusicCollections();

    return numberOfCollectionsFound;
}

Array<Common::CollectionOfSomeSort> availableCollectionsWithPerCollectionProgressCallBack(const std::function<void(void)>& callback)
{
    MutableArray<Common::CollectionOfSomeSort> results;

    results.appendObjectsFrom(RekordBuddy::availableRekordBuddyCollectionsWithPerCollectionProgressCallBack(callback));
    results.appendObjectsFrom(Rekordbox::availableRekordboxCollectionsWithPerCollectionProgressCallBack(callback));
    results.appendObjectsFrom(Serato::availableSeratoCollectionsWithPerCollectionProgressCallBack(callback));
    results.appendObjectsFrom(Traktor::availableTraktorCollectionsWithPerCollectionProgressCallBack(callback));
    results.appendObjectsFrom(PCDJ::availablePCDJCollectionsWithPerCollectionProgressCallBack(callback));
    results.appendObjectsFrom(AppleMusic::availableAppleMusicCollectionsWithPerCollectionProgressCallBack(callback));

    return results;
}

} }
