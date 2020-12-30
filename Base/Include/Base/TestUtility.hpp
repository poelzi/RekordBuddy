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

#pragma once

#if defined(NXA_BUILD_FOR_TESTING)
namespace NxA {

// -- Macros

// -- This helps make mocked methods virtual for testing when they are usually non-virtual.
// -- This is not ideal as it change the resulting code quite drastically but it will have to do until we find another solution.
#define NXA_VIRTUAL_FOR_TESTING                                     virtual

// -- This can be used to set a default return value for a mocked object's method. It also lets any number of calls
// -- take place which is what we want in a strictly mocked object (detect calls that we don't mock but let ones like this slide).
#define NXA_DEFAULT_RETURN_ON_CALL(object, method, value)           ON_CALL(object, method).WillByDefault(testing::Return(value)); \
                                                                    EXPECT_CALL(object, method).Times(testing::AnyNumber())

// -- This can be used to set a default return value for a mocked object's method. It also lets any number of calls
// -- take place which is what we want in a strictly mocked object (detect calls that we don't mock but let ones like this slide).
#define NXA_DEFAULT_RETURN_REF_ON_CALL(object, method, value)       ON_CALL(object, method).WillByDefault(testing::ReturnRef(value)); \
                                                                    EXPECT_CALL(object, method).Times(testing::AnyNumber())

// -- This can be used to set a default return value for a mocked object's method. It also lets any number of calls
// -- take place which is what we want in a strictly mocked object (detect calls that we don't mock but let ones like this slide).
#define NXA_DEFAULT_RETURN_BY_MOVE_ON_CALL(object, method, value)   ON_CALL(object, method).WillByDefault(testing::Return(testing::ByMove(value))); \
                                                                    EXPECT_CALL(object, method).Times(testing::AnyNumber())

// -- This can be used to set a default return value for a mocked object's method. It also lets any number of calls
// -- take place which is what we want in a strictly mocked object (detect calls that we don't mock but let ones like this slide).
#define NXA_DEFAULT_INVOKE_ON_CALL(object, method, lambda)          ON_CALL(object, method).WillByDefault(testing::Invoke(lambda)); \
                                                                    EXPECT_CALL(object, method).Times(testing::AnyNumber())

// -- This can be used to let one or many calls take place which is what we want in a strictly mocked object
// -- (detect calls that we don't mock but let ones like this slide).
#define NXA_EXPECT_ONE_CALL(object, method)                         EXPECT_CALL(object, method).Times(testing::AtMost(1))
#define NXA_EXPECT_SPECIFIC_NUMBER_CALLS(object, method, times)     EXPECT_CALL(object, method).Times(testing::AtMost(times))
#define NXA_EXPECT_MANY_CALLS(object, method)                       EXPECT_CALL(object, method).Times(testing::AnyNumber())

}
#else
#define NXA_VIRTUAL_FOR_TESTING
#endif
