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

#include <Base/CrashLog.hpp>

#include <Base/Assert.hpp>
#include <Base/Blob.hpp>
#include <Base/EventLog.hpp>
#include <Base/HttpRequest.hpp>
#include <Base/Platform.hpp>
#include <Base/String.hpp>
#include <Base/Threading.hpp>
#include <Base/Time.hpp>

#include "nxa_build_defines.h"

#include <windows.h>
#include <cwctype>
#include <psapi.h>

#if !defined(DEBUG)
#define CRASH_LOG_ENABLED
#endif

using namespace NxA;

// -- Private Variables

static String p_fileName;
static count p_lineNumber = 0;
static String p_methodName;
static String p_stackTrace;
static String p_bundleID;

static String p_apiKey;
static String p_userID;
static String p_appVersion;

static MutableArray<std::tuple<Time, String>> p_breadCrumbs;
static MutableArray<std::tuple<String, String>> p_userInfo;

// -- Private Functions

static HMODULE p_getModule(HANDLE pHandle)
{
    HMODULE hMods[1024];
    DWORD cbNeeded;

    if (EnumProcessModules(pHandle, hMods, sizeof(hMods), &cbNeeded)) {
        for (count i = 0; i < (cbNeeded / sizeof(HMODULE)); i++) {
            TCHAR szModName[MAX_PATH];
            if (GetModuleFileNameEx(pHandle, hMods[i], szModName, sizeof(szModName) / sizeof(TCHAR))) {
                String moduleName{ szModName };
                if (moduleName.hasPostfix("Rekord Buddy.exe")) {
                    return hMods[i];
                }
            }
        }
    }

    return nullptr;
}

static String p_getStrackTrace(count framesToSkip = 0)
{
    HANDLE process = GetCurrentProcess();
    auto module = p_getModule(process);
    if (module == nullptr) {
        return String{ "Unknown" };
    }

    constexpr count max_stack_depth = 100;
    void* stack[max_stack_depth];
    unsigned short frames = CaptureStackBackTrace(framesToSkip + 1, max_stack_depth, stack, NULL);

    MutableString result;
    for(count i = 0; i < frames; ++i) {
        if (i != 0) {
            result.append("|"_String);
        }

        result.append(String::stringWithFormat("%d-%x", frames - i - 1, (uinteger64)(stack[i]) - (uinteger64)(module)));
    }

    return (result);
}

static void p_notify(const String& type, const String& notification)
{
#if defined(CRASH_LOG_ENABLED)
    // -- The crashlog API must have been initialized first.
    if (!p_apiKey.length() || !p_appVersion.length()) {
        return;
    }

    MutableString jsonData{ "{\n"
                                          "  \"apiKey\": \"" };
    jsonData.append(p_apiKey);
    jsonData.append(String{ "\",\n"
                "  \"payloadVersion\": \"5\",\n"
                "  \"notifier\": {\n"
                "    \"name\": \"Rekord Buddy CrashLog\",\n"
                "    \"version\": \"1.0\",\n"
                "    \"url\": \"https://rekordbuddy.org\"\n"
                "  },\n"
                "  \"events\": [\n"
                "    {\n"
                "      \"exceptions\": [\n"
                "        {\n"
                "          \"errorClass\": \"" });
    jsonData.append(type);
    jsonData.append(String{ "\",\n"
                "          \"message\": \"Something unexpected happened.\",\n"
                "          \"stacktrace\": [\n"
                "            {\n"
                "              \"file\": \"" });
    jsonData.append(p_fileName);
    jsonData.append(String{ "\",\n"
                "              \"lineNumber\": " });
    jsonData.append(String::stringWithFormat("%llu", p_lineNumber));
    jsonData.append(String{ ",\n"
                "              \"method\": \"" });
    jsonData.append(p_methodName);
    jsonData.append(String{ " - " } );
    jsonData.append(p_stackTrace);
    jsonData.append(String{ "\"\n"
                "            }\n"
                "          ],\n"
                "          \"type\": \"windows\"\n"
                "        }\n"
                "      ],\n" });
    if (p_breadCrumbs.length() != 0) {
        jsonData.append(String{ "      \"breadcrumbs\": [\n" });
        for (count index = 0; index < p_breadCrumbs.length();) {
            auto [timeItHappened, message] = p_breadCrumbs[index];
            jsonData.append(String{ "        {\n"
            "          \"timestamp\": \"" });
            jsonData.append(timeItHappened.stringValueInGMTTimeZoneAsIso8601());
            jsonData.append(String{ "\",\n"
            "          \"name\": \"manual\",\n"
            "          \"type\": \"manual\",\n"
            "          \"metaData\": {\n"
            "            \"message\": \"" } );
            jsonData.append(message);
            if (++index != p_breadCrumbs.length()) {
                jsonData.append(String{ "\"\n"
                                                      "          }\n"
                                                      "        },\n" } );
            }
            else{
                jsonData.append(String{ "\"\n"
                                                      "          }\n"
                                                      "        }\n" } );
            }
        }
        jsonData.append(String{ "      ],\n" } );
    }
    jsonData.append(String{ "      \"context\": \"" });
    jsonData.append(notification);
    jsonData.append(String{ "\",\n"
                "      \"groupingHash\": \"" });
    jsonData.appendStringWithFormat("%s:%llu:%s", p_fileName.asUTF8(), p_lineNumber, p_methodName.asUTF8());
    jsonData.append(String{ "\",\n"
                "      \"unhandled\": false,\n"
                "      \"severity\": \"error\",\n"
                "      \"user\": {\n"
                "        \"id\": \"" });
    jsonData.append((p_userID.length() != 0) ? p_userID : "2323"_String);
    jsonData.append(String { "\",\n"
                "        \"name\": \"App User\",\n"
                "        \"email\": \"mruser@rekordbuddy.org\"\n"
                "      },\n"
                "      \"app\": {\n"
                "        \"id\": \"" });
    jsonData.append(p_bundleID);
    jsonData.append(String { "\",\n"
                "        \"version\": \"" });
    jsonData.append(p_appVersion);
    jsonData.append(String { "\",\n"
#if defined(NXA_BETA_BUILD)
                "        \"releaseStage\": \"beta\",\n"
#else
                "        \"releaseStage\": \"production\",\n"
#endif
                "        \"binaryArch\": \"x86_64\"\n"
                "      },\n"
                "      \"metaData\": {\n" });
    if (p_userInfo.length() != 0) {
        jsonData.append(String{ "        \"info\": {\n" });
        for (count index = 0; index < p_userInfo.length();) {
            auto [key, info] = p_userInfo[index];
            jsonData.append(String::stringWithFormat("\"%s\": \"%s\"",
                                                     key.asUTF8(),
                                                     info.asUTF8()));
            if (++index != p_userInfo.length()) {
                jsonData.append(String{ ",\n" });
            }
            else {
                jsonData.append(String{ "\n" });
            }
        }
        jsonData.append(String{ "        }\n" });
    }
    jsonData.append(String{ "      }\n"
                "    }\n"
                "  ]\n"
                "}" });
    HttpRequest::requestForURL(String{ "https://notify.bugsnag.com/" },
                               HttpRequest::Asynchronous::Yes,
                               Array<String>{ String{ "Content-Type: application/json" },
                                              String::stringWithFormat("Bugsnag-Api-Key: %s", p_apiKey.asUTF8()),
                                              String{ "Bugsnag-Payload-Version: 5" } },
                               jsonData);
#endif
}

// -- Class Methods

void CrashLog::initWithAPIKeyAndAppVersion(const String& key, const String& appVersion)
{
#if !defined(CRASH_LOG_ENABLED)
    NXA_DLOG("CrashLog is disabled.");
#endif

    p_apiKey = key;
    p_appVersion = appVersion;
}

void CrashLog::setUserID(const String& userID)
{
    p_userID = userID;
}

void CrashLog::setFileMethodNamesAndLineNumber(const char* fileName, const char* methodName, count lineNumber)
{
    p_stackTrace = p_getStrackTrace(1);
    p_fileName = String{ fileName }.stringByReplacingOccurencesOfWith("\\", "/");
    p_methodName = String{ methodName };
    p_lineNumber = lineNumber;
}

void CrashLog::setAppBundleID(const String& bundleID)
{
    p_bundleID = bundleID;
}

NxA::boolean CrashLog::hasCrashedInLastSession()
{
    return false;
}

const AssertionFailed& CrashLog::haltWith(const AssertionFailed& exception)
{
#if defined(CRASH_LOG_ENABLED)
    p_notify("UnexpectedIssue", { exception.what() });

    Threading::runOnMainThread([]() {
        Platform::alertWithTextAndInformativeText("Something wonderful has happened. You have found an unhandled issue."_String,
                                                  "We have been notified and will look into it. Rekord Buddy now will exit to preserve your data."_String);
    }, Threading::Blocking::Yes);

    exit(0);
#else
    return exception;
#endif
}

void CrashLog::notify(const String& notification)
{
    p_notify("Notification", notification);
}

void CrashLog::addUserInfoWithKey(const String& info, const String& key)
{
    p_userInfo.emplaceAppend(key, info);
}

void CrashLog::addBreadCrumb(const String& message)
{
#if !defined(CRASH_LOG_ENABLED)
    printf("Breadcrumb: %s\n", message.asUTF8());
#endif

    p_breadCrumbs.emplaceAppend(Time::currentTime(), message);
}
