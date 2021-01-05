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

#include <Persistence/PersistentSQLiteTransaction.hpp>

#include <SQLiteCpp/SQLiteCpp.h>

#include <Base/Base.hpp>
#include <Base/Threading.hpp>

#include <utility>

using namespace NxA;
static std::mutex p_globalMutex;

Transaction::Transaction(std::shared_ptr<SQLite::Database> withDb) : p_db{ std::move(withDb) }, p_globalLock{ std::make_unique<std::lock_guard<std::mutex>>(p_globalMutex) }
#if defined(NXA_PRINT_TRANSACTION_DURATION)
  , measure{Duration::startMeasuringDuration()}
#endif
{
    do {
        try {
            p_db->exec("begin immediate transaction");
            return;
        }
        catch (SQLite::DatabaseBusy&) {
            // -- We should try again in a little while
            Threading::sleepInMilliseconds(10);
        }
        catch (SQLite::Exception& e) {
            NXA_DLOG_WITH_FORMAT("Exception trying to begin transaction: %s", e.what());
            return;
        }
    }
    while (true);
}

Transaction::Transaction(Transaction&& from) noexcept : p_db{ std::exchange(from.p_db, nullptr) }, p_globalLock{ std::exchange(from.p_globalLock, nullptr) }
#if defined(NXA_PRINT_TRANSACTION_DURATION)
  , measure{ from.measure }
#endif
{
}

Transaction::~Transaction() noexcept
{
    if (this->p_db == nullptr) {
        return;
    }

    do {
        try {
            p_db->exec("ROLLBACK");
#if defined(NXA_PRINT_TRANSACTION_DURATION)
            NXA_DLOG_WITH_FORMAT("ROLLBACK Transaction that ran for %lldms", this->measure.duration().toMilliseconds());
#endif
            return;
        }
        catch (SQLite::DatabaseBusy&) {
            // -- We should try again in a little while
            Threading::sleepInMilliseconds(10);
        }
        catch (SQLite::Exception& e) {
            NXA_DLOG_WITH_FORMAT("Exception rolling back: %s", e.what());
            return;
        }
    }
    while (true);
}

void Transaction::commit()
{
    Duration retryDelay{10_Milliseconds};
#if defined(NXA_PRINT_TRANSACTION_DURATION)
    integer retryCount{ 0 };
#endif

    do {
        auto localdb = std::exchange(this->p_db, nullptr);
        try {
            if (localdb) {
                localdb->exec("COMMIT");
#if defined(NXA_PRINT_TRANSACTION_DURATION)
                NXA_DLOG_WITH_FORMAT("COMMIT Transaction that ran for %lldms (%d retries)", this->measure.duration().toMilliseconds(), retryCount);
#endif
                return;
            }
            throw SQLite::Exception{ "Transaction already commited or has been moved to another variable; can not commit." };
        }
        catch (SQLite::DatabaseBusy& e) {
            // -- we should try again in a little while; busy-wait with exponential backoff
            Threading::sleepInMilliseconds(retryDelay.toMilliseconds());
            retryDelay = 10_Milliseconds + retryDelay.divideIntoParts<4>();
            std::exchange(this->p_db, localdb);
        }
        catch (SQLite::Exception& e) {
            NXA_DLOG_WITH_FORMAT("Exception in transaction: %s", e.what());
            return;
        }

#if defined(NXA_PRINT_TRANSACTION_DURATION)
        ++retryCount;
#endif
    }
    while (true);
}

Transaction& Transaction::operator=(Transaction&& from) noexcept
{
    this->p_db = std::exchange(from.p_db, nullptr);
    return *this;
}
