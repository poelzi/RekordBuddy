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

#define NXA_CAT_SUB_MACRO(A, B) A ## B
#define NXA_CAT(A, B) NXA_CAT_SUB_MACRO(A, B)

#if !defined(NXA_OBJECT_INTERNAL_CLASS)
    #if defined(NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE)
        #define NXA_OBJECT_INTERNAL_PERSISTENT_CLASS NXA_CAT(Persistent, NXA_OBJECT_CLASS)
        #define NXA_OBJECT_INTERNAL_CLASS NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE::NXA_OBJECT_INTERNAL_PERSISTENT_CLASS
    #else
        #define NXA_OBJECT_INTERNAL_CLASS NXA_CAT(NXA_OBJECT_CLASS, Internal)
    #endif
#endif

#if defined(NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE)
    inline namespace NXA_OBJECT_INTERNAL_CLASS_IS_PERSISTENT_IN_NAMESPACE {
        class NXA_OBJECT_INTERNAL_PERSISTENT_CLASS;
    }
#else
    class NXA_OBJECT_INTERNAL_CLASS;
#endif

#if defined(NXA_OBJECT_BASE_CLASS)
    #define NXA_OBJECT NXA_OBJECT_BASE_CLASS
#else
    #define NXA_OBJECT std::shared_ptr<NXA_OBJECT_INTERNAL_CLASS>
#endif
