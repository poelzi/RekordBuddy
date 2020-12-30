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

#include <Base/Assert.hpp>
#include <Base/Map.hpp>
#include <Base/Pointers.hpp>
#include <Base/String.hpp>
#include <Base/Volume.hpp>

namespace NxA {

// -- Class Variables

#if defined(NXA_BUILD_FOR_TESTING)
WeakReference<Array<Volume>> Volume::p_testPathsOfAvailableVolumes;
WeakReference<Volume> Volume::p_testBootVolume;
WeakReference<String> Volume::p_testHomeVolumeName;
#if defined(NXA_PLATFORM_WINDOWS)
WeakReference<Map<String, Volume>> Volume::p_testVolumeNamesToDriveLetter;
#endif
#endif

}
