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

#include <Base/HttpRequest.hpp>
#include <Base/Semaphore.hpp>

#import <Cocoa/Cocoa.h>

using namespace NxA;

// -- Class Methods

Optional<String> HttpRequest::maybeStringWithContentsOfURL(const String& url)
{
    NSError* error = nil;
    NSString* cocoaString = [NSString stringWithContentsOfURL:[NSURL URLWithString:[NSString stringWithUTF8String:url.asUTF8()]]
                                                                          encoding:NSUTF8StringEncoding
                                                                             error:&error];
    if (error != nil) {
        return nothing;
    }

    return String::stringWithUTF8(cocoaString.UTF8String);
}

void HttpRequest::requestForURL(const String& url, Asynchronous asynchronous, Optional<Array<String>> maybeHeaders, Optional<String> maybeData)
{
    // -- TODO: To be implemented on macOS.
    NXA_ASSERT_FALSE(maybeHeaders.isValid());
    NXA_ASSERT_FALSE(maybeData.isValid());

    NSMutableURLRequest* request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithUTF8String:url.asUTF8()]]];

    if (asynchronous == Asynchronous::Yes) {
        NSURLSessionDataTask* task = [[NSURLSession sharedSession] dataTaskWithRequest:request];
        [task resume];
    }
    else {
        Semaphore requestPosted;
        NotNull<Semaphore*> requestPostedAddress{ &requestPosted };

        NSURLSessionDataTask* task = [[NSURLSession sharedSession] dataTaskWithRequest:request
                                                                     completionHandler:^(NSData *data, NSURLResponse *response, NSError *error) {
                                                                         requestPostedAddress->notify();
                                                                     }];

        [task resume];

        requestPosted.wait();
    }
}

std::tuple<HttpRequest::Result, String> HttpRequest::postSynchronousRequestForURL(const String& url, const String& postString)
{
    NSMutableURLRequest *request = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithUTF8String:url.asUTF8()]]];
    [request setHTTPMethod:@"POST"];
    [request setHTTPBody:[[NSString stringWithUTF8String:postString.asUTF8()] dataUsingEncoding:NSUTF8StringEncoding]];
    [request setTimeoutInterval:10];

    NXA_DLOG_WITH_FORMAT("POST at %s with %s", url.asUTF8(), postString.asUTF8());

    Semaphore requestPosted;
    NotNull<Semaphore*> requestPostedAddress{ &requestPosted };

    __block String resultingData;
    __block Result result = Result::Error;

    NSURLSessionDataTask* task = [[NSURLSession sharedSession] dataTaskWithRequest:request
            completionHandler:^(NSData* data,
                                NSURLResponse* response,
                                NSError* error) {
                if ((error == nil) && (data.length != 0)) {
                    resultingData = String::stringWithUTF8AndSizeInBytes(reinterpret_cast<const character*>(data.bytes),
                                                                         data.length);
                    result = Result::Success;
                }

                requestPostedAddress->notify();
            }];

    [task resume];

    requestPosted.wait();

    return { result, resultingData };
}
