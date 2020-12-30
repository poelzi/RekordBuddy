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

// -- This file contains macOS specific implementations.
static_assert(NXA_PLATFORM_MACOS, "Invalid platform.");

#include <Base/Array.hpp>
#include <Base/Platform.hpp>
#include <Base/String.hpp>

#include <Cocoa/Cocoa.h>
#include <Foundation/Foundation.h>

#include <sys/types.h>
#include <sys/sysctl.h>

using namespace NxA;

// -- Class Methods

Optional<String> Platform::maybeUniqueIDForComputer()
{
    static Optional<String> maybeUniqueIDCache;
    static boolean uniqueIDReadAlready = false;

    if (!uniqueIDReadAlready) {
        io_service_t platformExpert = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceMatching("IOPlatformExpertDevice"));
        if (platformExpert != 0) {
            auto serialNumberAsCFString = static_cast<CFStringRef>(IORegistryEntryCreateCFProperty(platformExpert, CFSTR(kIOPlatformSerialNumberKey), kCFAllocatorDefault, 0));
            if (serialNumberAsCFString != nil) {
                if (CFGetTypeID(serialNumberAsCFString) == CFStringGetTypeID()) {
                    NSString* result = [NSString stringWithFormat:@"%@", serialNumberAsCFString];

                    NSMutableString* resultAsString = [@"m" mutableCopy];

                    for (NSUInteger index = 0; index < result.length; ++index) {
                        [resultAsString appendString:[NSString stringWithFormat:@"%02hx", [result characterAtIndex:index]]];
                    }

                    maybeUniqueIDCache = String(resultAsString.UTF8String);
                }

                CFRelease(serialNumberAsCFString);
            }

            IOObjectRelease(platformExpert);
        }

        uniqueIDReadAlready = true;
    }

    return maybeUniqueIDCache;
}

Optional<String>& Platform::maybeUsername()
{
#if defined(NXA_BUILD_FOR_TESTING)
    if (Platform::p_testUserName.isValid()) {
        static Optional<String> maybeUsername{ *Platform::p_testUserName.get() };
        return maybeUsername;
    }
#endif

    static Optional<String> maybeUsername = (NSUserName().UTF8String != nullptr) ? Optional<String>{ { NSUserName().UTF8String } } : Optional<String>{ };
    return maybeUsername;
}

String Platform::osVersion()
{
    NSDictionary* sv = [NSDictionary dictionaryWithContentsOfFile:@"/System/Library/CoreServices/SystemVersion.plist"];
    return { [[sv objectForKey:@"ProductVersion"] UTF8String] };
}

String Platform::hardwareName()
{
    size_t len = 0;
    sysctlbyname("hw.model", NULL, &len, NULL, 0);
    if (!len) {
        return "Unknown"_String;
    }

    char* model = reinterpret_cast<char*>(malloc(len * sizeof(char)));
    sysctlbyname("hw.model", model, &len, NULL, 0);

    String result{ model };

    free(model);

    return result;
}

boolean Platform::isRunningFromInstallationFolder()
{
    NSString* appLocation = [[NSProcessInfo processInfo] arguments][0];
    return [appLocation hasPrefix:@"/Applications/"];
}

Array<String> Platform::namesOfOtherRunningApplications()
{
    MutableArray<String> results;

    for (NSRunningApplication* app in [[NSWorkspace sharedWorkspace] runningApplications]) {
        auto nameFound = app.localizedName.lowercaseString.UTF8String;
        if (nameFound) {
            results.emplaceAppend(nameFound);
        }
    }

    return { std::move(results) };
}

void Platform::alertWithTextAndInformativeText(const String& text, const String& informativeText)
{
    NSAlert* alert = [[NSAlert alloc] init];
    alert.messageText = [NSString stringWithUTF8String:text.asUTF8()];
    alert.informativeText = [NSString stringWithUTF8String:informativeText.asUTF8()];
    [alert addButtonWithTitle:@"Ok"];
    [alert runModal];
}
