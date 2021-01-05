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


#include <Base/Base.hpp>
#include <Base/Duration.hpp>
#include <mutex>

namespace SQLite {
    class Database;
}

namespace NxA {

    struct Transaction final
    {
        explicit Transaction(std::shared_ptr<SQLite::Database> withDb);
        Transaction(Transaction&& from) noexcept;
        ~Transaction() noexcept;
        void commit();
        Transaction() = delete;
        Transaction(const Transaction&) = delete;
        Transaction& operator=(const Transaction&) = delete;
        Transaction& operator=(Transaction&& from) noexcept;

    private:
        std::shared_ptr<SQLite::Database> p_db;
        std::unique_ptr<std::lock_guard<std::mutex>> p_globalLock;
#if defined(NXA_PRINT_TRANSACTION_DURATION)
        StartOfMeasurement measure;
#endif
    };

}
