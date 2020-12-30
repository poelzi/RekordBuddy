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

#include <Base/Threading.hpp>
#include <Base/Optional.hpp>

#if defined(NXA_PLATFORM_WINDOWS)
#include <Windows.h>
#else
#include <unistd.h>
#endif

#include <mutex>

using namespace NxA;

// -- Local Variables
static Optional<std::function<void(std::function<void()>, Threading::Blocking)>> p_maybeRunOnMainThreadFunctionToUse;
static Optional<std::function<NxA::boolean()>> p_maybeIsOnMainThreadFunctionToUse;
static Optional<std::function<void(count)>> p_maybeSleepInMillisecondsFunctionToUse;

// -- Class Methods

void Threading::setIsOnMainThreadFunctionToUse(std::function<NxA::boolean()>&& function)
{
    // -- This method itself, needs to be used on the main thread
    // -- and be called first when setting up threading.
    NXA_ASSERT_TRUE(function());

    p_maybeIsOnMainThreadFunctionToUse = { std::move(function) };
}

NxA::boolean Threading::isOnMainThread()
{
    // -- If nothing set a function to use already then something went wrong.
    NXA_ASSERT_TRUE(p_maybeIsOnMainThreadFunctionToUse.isValid());

    return (*p_maybeIsOnMainThreadFunctionToUse)();
}

void Threading::setRunOnMainThreadFunctionToUse(std::function<void(std::function<void()>&&, Blocking)>&& function)
{
    // -- This method itself, needs to be used on the main thread.
    NXA_ASSERT_TRUE(Threading::isOnMainThread());

    p_maybeRunOnMainThreadFunctionToUse = { std::move(function) };
}

void Threading::runOnMainThread(std::function<void()>&& function, Blocking blocking)
{
    if (isOnMainThread()) {
        function();
        return;
    }

    // -- If nothing set a function to use already then something went wrong.
    NXA_ASSERT_TRUE(p_maybeRunOnMainThreadFunctionToUse.isValid());

    (*p_maybeRunOnMainThreadFunctionToUse)(std::move(function), blocking);
}

void Threading::setSleepInMillisecondsFunctionToUse(std::function<void(count)>&& function)
{
    // -- This method itself, needs to be used on the main thread.
    NXA_ASSERT_TRUE(Threading::isOnMainThread());

    p_maybeSleepInMillisecondsFunctionToUse = { std::move(function) };
}

void Threading::sleepInMilliseconds(count milliseconds)
{
    // -- If nothing set a function to use already then something went wrong.
    NXA_ASSERT_TRUE(p_maybeSleepInMillisecondsFunctionToUse.isValid());

    (*p_maybeSleepInMillisecondsFunctionToUse)(milliseconds);
}

void Threading::setPassThruMethodsForAll()
{
    // -- We setup a threading glue code which assume we're always on the main thread.
    Threading::setIsOnMainThreadFunctionToUse([]() {
        return true;
    });

    Threading::setRunOnMainThreadFunctionToUse([](std::function<void(void)>&& function, Blocking) {
        function();
    });

    Threading::setSleepInMillisecondsFunctionToUse([](count milliseconds) {
#if defined(NXA_PLATFORM_MACOS)
        sleep(milliseconds);
#elif defined(NXA_PLATFORM_WINDOWS)
        Sleep(milliseconds);
#elif defined(NXA_PLATFORM_LINUX)
        usleep(milliseconds * 1000);
#else
        #error Unsupported platform.
#endif
    });
}