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

#include <Base/Duration.hpp>
#include <mutex>
#include <condition_variable>

namespace NxA {

class Semaphore final
{
private:
    using Mutex = std::mutex;
    Mutex p_mutex;
    std::condition_variable p_condition;
    unsigned long p_count = 0; // -- start locked

public:

    //! Indicate to anyone waiting (or will wait) on this semaphore that it is signalled
    void notify()
    {
        std::unique_lock<Mutex> lock{p_mutex};
        ++p_count;
        p_condition.notify_one();
    }

    //! Wait until someone has notified this semaphore
    void wait()
    {
        std::unique_lock<Mutex> lock{p_mutex};
        while(p_count == 0) {
            // -- Handle spurious wake-ups.
            p_condition.wait(lock);
        }
        --p_count;
    }

    //! wait for up to some duration. if semaphore was not signaled in that time, return false. if we woke up at any time within the duration, return true.
    bool waitFor(const Duration& d)
    {
        std::unique_lock<Mutex> lock{p_mutex};

        if (p_condition.wait_for(lock, d.toStdDuration<std::intmax_t, std::milli>(), [&]{ return p_count > 0; })) {
            --p_count;
            return true;
        }

        return false;
    }

    //! check if we are signalled, returning true (and unlocking) if so. Otherwise return false without blocking.
    bool tryWait()
    {
        std::unique_lock<Mutex> lock{p_mutex};
        if(p_count != 0) {
            --p_count;
            return true;
        }
        return false;
    }
};

}
