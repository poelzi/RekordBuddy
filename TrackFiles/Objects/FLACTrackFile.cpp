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

#include <TrackFiles/FLACTrackFile.hpp>

#include "Internal/FLACTrackFileInternal.hpp"
#include "Internal/OGGTrackFileInternal.hpp"
#include "Internal/ID3TrackFileInternal.hpp"

using namespace NxA;

#define NXA_OBJECT_CLASS                            FLACTrackFile
#define NXA_OBJECT_BASE_CLASS                       TrackFile

#include <Base/ObjectDefinition.ipp>

// -- Factory Methods

Optional<TrackFile> FLACTrackFile::maybeFileWithFileAt(const FilePath& path)
{
    FLACTrackFile file{ std::make_shared<Internal>(path) };

    if (!file->parse()) {
        return nothing;
    }

    return { file };
}

// -- Instance Methods

void FLACTrackFile::removeFieldNamedIfAny(const String& name)
{
    nxa_internal->removeFieldNamedIfAny(name);
}

void FLACTrackFile::removePrivateFramesOwnedByIfAny(const String& owner)
{
    nxa_internal->removePrivateFramesOwnedByIfAny(owner);
}
