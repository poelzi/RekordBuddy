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

// -- This file contains windows specific implementations.
static_assert(NXA_PLATFORM_WINDOWS, "Invalid platform.");

#include <Base/Platform.hpp>

#include <Base/Array.hpp>
#include <Base/Types.hpp>
#include <Base/String.hpp>

#include <Windows.h>
#include <Lmcons.h>
#include <VersionHelpers.h>

using namespace NxA;

// -- System API wrappers

static Optional<std::wstring> p_maybeGetRegistryString(HKEY hKey, const std::wstring& subKey, const std::wstring& value)
{
    DWORD dataSize{};
    LONG retCode = ::RegGetValueW(hKey, subKey.c_str(), value.c_str(), RRF_RT_REG_SZ, nullptr, nullptr, &dataSize);

    if (retCode != ERROR_SUCCESS) {
        return nothing;
    }

    std::wstring data;
    data.resize(dataSize / sizeof(wchar_t));

    retCode = ::RegGetValueW(hKey, subKey.c_str(), value.c_str(), RRF_RT_REG_SZ, nullptr, &data[0], &dataSize);

    if (retCode != ERROR_SUCCESS) {
        return nothing;
    }

    DWORD stringLengthInWchars = dataSize / sizeof(wchar_t);

    // -- Exclude the NUL written by the Win32 API
    stringLengthInWchars--;

    data.resize(stringLengthInWchars);

    return data;
}

// -- Class Methods

Optional<String> Platform::maybeUniqueIDForComputer()
{
    static Optional<String> maybeUniqueIDCache;
    static boolean uniqueIDReadAlready = false;

    if (!uniqueIDReadAlready) {
        auto maybeMachineID = p_maybeGetRegistryString(HKEY_LOCAL_MACHINE, L"SOFTWARE\\Microsoft\\Cryptography", L"MachineGuid");
        if (maybeMachineID.isValid()) {
            maybeUniqueIDCache = "W"_String.stringByAppending(String{ *maybeMachineID });

            uniqueIDReadAlready = true;
        }
    }

    return *maybeUniqueIDCache;
}

Optional<String>& Platform::maybeUsername()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Platform::p_testUserName.isValid()) {
        static Optional<String> maybeUsername{ *Platform::p_testUserName.get() };
        return maybeUsername;
    }
#endif

    static Optional<String> maybeUsername;
    if (!maybeUsername.isValid()) {
        char username[UNLEN+1];
        DWORD username_len = UNLEN+1;
        GetUserName(username, &username_len);

        maybeUsername = String::stringWithUTF8(username);
    }

    return maybeUsername;
}

NxA::boolean Platform::isRunningFromInstallationFolder()
{
    // -- TODO: TO be implemented.
    return true;
}

String Platform::osVersion()
{
    if (!IsWindows10OrGreater()) {
        return "10.0"_String;
    }

    return "unknown";
}

String Platform::hardwareName()
{
    // -- TODO: TO be implemented.
    return "Unknown"_String;
}

Array<String> Platform::namesOfOtherRunningApplications()
{
    // -- TODO: TO be implemented.
    return { };
}

void Platform::alertWithTextAndInformativeText(const NxA::String& text, const String& informativeText)
{
    MessageBox(NULL, String::stringWithFormat("%s\n\n%s", text.asUTF8(), informativeText.asUTF8()).asUTF8(), "Alert", MB_OK);
}

String Platform::GetLastError()
{
    DWORD error = ::GetLastError();
    if (error) {
        LPVOID lpMsgBuf;
        DWORD bufLen = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS, NULL, error,
                                     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&lpMsgBuf, 0, NULL);
        if (bufLen) {
            LPCSTR lpMsgStr = (LPCSTR)lpMsgBuf;
            std::string result(lpMsgStr, lpMsgStr + bufLen);

            LocalFree(lpMsgBuf);

            return String::stringWithFormat("%d:%s", error, result.c_str());
        }
        else {
            return String::stringWithFormat("%d:null", error);
        }
    }

    return { };
}