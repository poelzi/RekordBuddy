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

#if !defined(NXA_OBJECT_INTERNAL_CLASS)
    #if !defined(NXA_OBJECT_CLASS)
        #error Missing definition
    #endif

    #define NXA_CAT_SUB_MACRO(A, B) A ## B
    #define NXA_CAT(A, B) NXA_CAT_SUB_MACRO(A, B)
    #define NXA_OBJECT_INTERNAL_CLASS NXA_CAT(NXA_OBJECT_CLASS, Internal)
#endif

public:
    using Internal = NXA_OBJECT_INTERNAL_CLASS;

    #if !defined(NXA_INTERNAL_OBJECT_FOR)
        #define NXA_INTERNAL_OBJECT_FOR(other) static_cast<Internal*>(other.get())
    #endif

    // -- Class Methods
    #if defined(NXA_OBJECT_CLASS)
        static NXA_OBJECT_CLASS toWrapperClass(std::shared_ptr<Internal>&& from)
        {
            // -- If we're moving this 'from', it can't be referred to by anyone else so we should now just be referred by one.
            NXA_ASSERT_TRUE(from.use_count() == 1);

            return std::move(from);
        }
        static NXA_OBJECT_CLASS toWrapperClass(std::shared_ptr<Internal>& from)
        {
            return { from };
        }
        static const Internal& fromWrapperClass(const NXA_OBJECT_CLASS& from)
        {
            return *NXA_INTERNAL_OBJECT_FOR(from);
        }
        static Internal& fromWrapperClass(NXA_OBJECT_CLASS& from)
        {
            return *NXA_INTERNAL_OBJECT_FOR(from);
        }
    #endif

    // -- Instance Methods
    #if !defined(NXA_INTERNAL_OBJECT_DOES_NOT_NEED_EQUAL_OPERATOR)
        #if defined(NXA_OBJECT_INTERNAL_BASE_CLASS)
            bool operator==(const NXA_OBJECT_INTERNAL_BASE_CLASS& other) const noexcept override
            {
                if (typeid(*this) != typeid(other)) {
                    return false;
                }

                return dynamic_cast<const Internal*>(&other)->operator==(*this);
            }
            bool operator<(const NXA_OBJECT_INTERNAL_BASE_CLASS& other) const noexcept override
            {
                if (typeid(*this) != typeid(other)) {
                    return false;
                }

                return dynamic_cast<const Internal*>(&other)->operator<(*this);
            }
        #elif defined(NXA_INTERNAL_OBJECT_IS_PURE_VIRTUAL)
            virtual bool operator==(const NXA_OBJECT_INTERNAL_CLASS& other) const noexcept = 0;
            virtual bool operator<(const NXA_OBJECT_INTERNAL_CLASS& other) const noexcept = 0;
        #endif
    #endif

#undef NXA_CAT_SUB_MACRO
#undef NXA_CAT
#undef NXA_OBJECT_CLASS
#undef NXA_OBJECT_INTERNAL_CLASS
#undef NXA_OBJECT_INTERNAL_BASE_CLASS
#undef NXA_INTERNAL_OBJECT_IS_PURE_VIRTUAL
#undef NXA_INTERNAL_OBJECT_DOES_NOT_NEED_EQUAL_OPERATOR
