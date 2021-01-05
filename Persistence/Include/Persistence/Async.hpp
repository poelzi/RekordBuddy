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

#include <Base/Types.hpp>
#include <mutex>
#include <future>

#if defined(NXA_PLATFORM_MACOS)
#include <dispatch/dispatch.h>
#endif

namespace NxA {

#if defined(NXA_PLATFORM_MACOS)

template <typename Function, typename... ArgumentPack>
decltype(auto) async(Function&& asyncFunction, ArgumentPack&&... argumentPack)
{
#if defined(__has_feature) && __has_feature(address_sanitizer)
    // disable async threading for address_sanitizer
    return std::async(std::launch::deferred, std::forward<Function>(asyncFunction), std::forward<ArgumentPack>(argumentPack)...);
#else
    using PackagedTask = std::packaged_task<typename std::invoke_result<Function, ArgumentPack...>::type()>;
    auto* packagedTask = new PackagedTask{std::bind(std::forward<Function>(asyncFunction), std::forward<ArgumentPack>(argumentPack)...)};
    auto result = packagedTask->get_future();
    auto queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);

    // -- on apple platforms we prefer to share the GCD queue since their std::async fires up a thread every time, which is slow.
    dispatch_async_f(queue, packagedTask, [](void* asyncFunction_) {
        auto packagedTaskPointer = std::unique_ptr<PackagedTask>(static_cast<PackagedTask*>(asyncFunction_));
        (*packagedTaskPointer)();
    });

    return result;
#endif
}

#else

// -- We're free to forward to platform async since most other platforms use a work queue for it's async implementation
template <typename Function, typename... ArgumentPack>
decltype(auto) async(Function&& asyncFunction, ArgumentPack&&... argumentPack)
{
    return std::async(std::launch::async, std::forward<Function>(asyncFunction), std::forward<ArgumentPack>(argumentPack)...);
}

#endif

template <typename R, typename Iterator, typename Function>
std::vector<std::future<R>> divideWork(Iterator begin, Iterator end, count numberOfElements, count numberOfChunks, Function f)
{
    std::vector<std::future<R>> result;

    if (numberOfChunks == 0) {
        return result;
    }

    result.reserve(numberOfChunks);

    if (numberOfElements < 1000) {
        std::promise<R> promise;
        result.emplace_back(std::move(promise.get_future()));
        promise.set_value(f(begin, end));
        return result;
    }

    auto chunk = numberOfElements / numberOfChunks;
    auto remainder = numberOfElements % numberOfChunks;

    for (count i = 0; i < numberOfChunks - 1; ++i) {
        auto nextEnd = std::next(begin, chunk + (remainder ? 1 : 0));
        result.emplace_back(std::move(NxA::async(f, begin, nextEnd)));

        begin = nextEnd;
        if (remainder) {
            remainder -= 1;
        }
    }

    result.emplace_back(std::move(NxA::async(f, begin, end)));
    return result;
}

}
