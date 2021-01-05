![](Resources/wtfm.jpg)
# Introduction

This documents describes the Base library coding standards. At no point does this pretend to be the end-all/be-all of coding standards. Some of it comes from years of learning the craft, some of it is arbitrary and was only chosen because of personal preferences.

Either way, it is the law of the land and must be strictly adhered to when contributing code to Base or to projects using Base. This document can be modified at times to reflect any changes or evolutions.

Some of the existing code might not correctly enforce/use the coding standards. It is understood that if the code and this document differ, this document is the prevailing authority and the code should be updated.

# Overall Philosophy

The main points dictating the overall philosophy for the Base library are:

* **Be consistent:** Sticking to this document will help in that respect but in general, stick to one style/method/process. Code should be easy to navigate because it should all be consistent and familiar.
* **Fail early:** If something has gone wrong or is not the way it should be, don't assume calling code will deal with the situation. Fail early and fail hard to prevent unpredictable states or behaviour (see "Dead Programs Tell No Lies" in [HUN99] page 120).
* **Make your code defensive**: Don't assume any state, any input or any return values. Unless something is documented and guaranteed to work a specific way, test it and assert to make sure it is (see "Assertive Programming" in [HUN99] page 122). Assertions should ALWAYS be on, even in Release builds.
* **Don't let your API be used incorrectly:** Method signatures should be non-ambiguous to make sure users of your classes cannot use them incorrectly. Be verbose, be descriptive and don't allow arguments to have multiple meanings/uses.
* **Don't live with broken windows:** Formatting, naming and any other standards can be a pain sometimes. But committing code that doesn't adhere to the standard or that contains some hack of some sort is an invitation for others to think it's ok to do it too. Fix broken windows before you commit your code (see "Software Entropy" in [HUN99] page 4).
* **Don't repeat yourself:** Duplicated code makes it harder to debug, optimise and read the code. Fixes and optimisations have to be applied in multiple places and inlined copies of the same code could be made easier to read by each calling the same method instead. (see "The Evils of Duplication" in [HUN99] page 26).

# Style & Formatting

### Files

* **Header files** use the `.hpp` extension.
* **Source files** use the `.cpp` extension.

##### All files should have a standard header at the top.

This is the standard header used by the Base library:

```
//
//    <module name> - <description>
//    Copyright (C) 2020-2021 Didier Malenfant (didier@rekordbuddy.org)
//
//    This program is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
```

### Naming conventions

* **Variables and methods** use the `lowerCamelCase` convention unless compatibility with an unwrapped standard library api/protocol is required.
* **Class names and namespaces** use the `UpperCamelCase`.
* **Compiler macros** use the `UPPER_CASE_SNAKE` convention.
* **constexpr and enum names** should use the `lowerCamelCase` convention.
* **Use meaningful, pronounceable and intention revealing names for methods and variables.** Method names should always read like actions and variables like object names (see "Meaningful Names" in [MAR09] page 17). This does not apply when providing implementations of unwrapped standard library api/protocols since we don't control the names in that case.
* **Don't use short form or single letter names.** Everything should be spelled out.
* **Methods names should try to explain the arguments used after them.** C++ unfortunately doesn't support named arguments. This makes code very hard to read when looking at a calling method since arguments after the first one do not have any context. For this, we try to add the description in the method name itself. This can get pretty hard to read past a certain number of arguments which a good reason for methods to only have up to two or three arguments if possible (See below in the **Coding Conventions** section).

```
    // --- Like this
    void processElementAndLogMessagesIn(const Element& element, MessageLog& log)
    {
        integer startIndex = 0;
        for (auto index = startIndex; index < 12; ++index) {
            ...
        }
    }

    // --- NOT like this
    void process(const Element& element, MessageLog& log)
    {
        integer idx = 0;
        for (auto i = idx; i < 12; ++i) {
            ...
        }
    }

```

### Whitespace

* **Tabs** are 4 characters and should be spaces only.
* **Line-endings** should be OS X/UNIX (LF).
* **Last line of all files should be a newline.** Some compilers, like clang, complain if that's not the case.
* **Pointer and reference symbols** should be attached to the type:

```
    MyClass& myVar;         // --- Like this
    character* otherVar = nullptr;
    MyType& aMethod(MyOtherType& aVar);

    MyClass &myVar;         // --- NOT like this
    character *otherVar = nullptr;
    MyType &aMethod(MyOtherType &aVar);
```

### Code Layout

* **`{ }` are always required for blocks**, even single lines or switch statements.
* **class definition and method declaration** use curly brackets on separate lines:

```
    class MyClass
    {
        ...
    }

    void myMethod()
    {
        ...
    }
```

* **statements** use an open curly bracket on the same like as the statement:

```
    if (true) {
        ...
    }
    else {
        ...
    }

    while (1) {
        ...
    }

    for (auto& var : vars) {
        ...
    }

    switch (c) {
        case 0: {
            ...
            break;
        }
        case 1: {
            ...
            break;
        }
        default: {
            ...
        }
    }
```

* **`return` statements** don't use any parenthesis.

```
    // --- Like this
    return localLog.count == 3;

    // --- NOT like this
    return (localLog.count == 3);
```
* **Comments are used to separate code sections** both in the headers and the implementation. In headers the order of declaration is:

```
    class MyClass : NxA::Object
    {
        // -- Class Methods
        ...

        // -- Constructors & Destructors
        ...

        // -- Operators
        ...

        // -- Instance Methods  
        ...
    }
```

* **Do not to declare multiple references or raw pointers on one line**, even if they have the same type. C++ makes this visually go against having the `&` or `*` symbols being attached to the type name and not the variable. Regular local variables (i.e. not references or raw pointers) are fine to be defined on one line.

```
    // --- Like this
    Some<LargeType<With<Lots>>, Of<args>> a, b;
    Type& oneReference;
    Type& anotherReference;

    // --- NOT like this
    Type& oneReference, &anotherReference;
```

* **Keep vertical distance short.** Concepts that are closely related should be kept close together. This takes precedence over line length. While being able to compare files side by side is useful is a few limited cases, we prefer to **NOT** to put a hard limit on line length. A soft limit is set at 120 characters but again, vertical distance for readability is more important that artificially staying below this limit (see "Vertical Distance" in [MAR09] page 80).
* **When local variables are used in `if()`, `for()` or `return` statements**, they should be initialised on the line right above those statements, if possible, with no blank line between the two.

```
    // --- Like this
    auto messages = localLog.messages;
    for (auto&& message : messages) {
        auto text = message.text;
        if (text.length) {
            ...
        }
    }

    auto count = localLog.count;
    printf("%ld", count);

    auto result = localLog.warningAsString;
    return result;

    // --- NOT like this
    auto count = localLog.count;
    auto messages = localLog.messages;

    for (auto&& message : messages) {
        auto text = message.text;

        if (text.length) {
            ...
        }
    }

    printf("%ld", count);

    return localLog.warningAsString;
```

* **Limit Variable Scope.** Similarly to trying to limit vertical distance, objects should not stay around longer than they need to so variables should always be declared as late as possible. Any variables only used inside a given scope should be declared inside that scope, unless the scope is a loop and the variable's value is unaffected by the loop in which case it is more optimal to declare the object outside of the loop.

```
    // --- Like this
    auto formatter = Formatter();
    for (auto&& text : strings) {
        auto formattedVersion = formatter.format(test);
        ...
    }

    // --- NOT like this
    auto formattedVersion = String::emptyString();
    for (auto&& text : strings) {
        auto formatter = Formatter();

        formattedVersion = formatter.format(test);
        ...
    }
```

* **Local variables can be used to store returned values** from methods to help document the code. Naming the variable can help the reader understand what intent is and contributes to make the code read like a sentence. To make sure performance is not an issue, make sure to `move` the value once you pass it down to the method. This also clarifies that the local variable is only there for documentation and not re-used anywhere else.

```
    // --- Like this
    auto formattedText = String::formatString("Hello");
    log.addMessage(std::move(formattedText));

    // --- NOT like this
    log.addMessage(String::formatString("Hello"));
```

* **Organize code so that it reads top to bottom.** As much as possible, in both the declarations and definitions and within each code section described above, the code should be organised so that main methods are found at the top and sub-methods are found underneath them (see "Vertical Ordering" in [MAR09] page 84).

### Includes

* **Header files** should always contains a `#pragma once` statement at the top, underneath the file header.
* `<>` should be used for includes paths, always including the top folder/module name for the include file.

```
    // --- Like this
    #include <Base/String.hpp>

    // --- NOT like this
    #include "String.hpp"
```

* **Includes should be placed in reverse order of locality** with other modules/libs first, same library/module second and finally system includes last. A blank line should separate each type of include. This helps insure that application headers are more likely to include any dependencies/forward declarations they need and not get those dependencies resolved by chance from previous include statements.
* **Group includes by module** and separate each group by a blank line.  
* **Each module/library should have a single include file**, named after the module or library and which includes all the relevant include files for that module/library.
* **Forward declare** in header files, underneath all include statements, when possible instead of including to reduce include complexity.
* For example, in a file from the `Persistence` library:

```
    #include <Persistence/PersistentObject.hpp>

    #include <Base/DecimalNumber.hpp>
    #include <Base/String.hpp>

    #include <stdio.h>

    // -- Forward Declarations
    class PersistentContext;    
```

### Comments

* **Code comments should use `// -- My comment.`** and not the block notation `/* ... */`. This allows for easily commenting out block of code during development.
* **Avoid using comments** when possible (see 'Comments' in the **Code Conventions** section below).

# Code Conventions

### Namespaces

* Always at least use the `NxA` namespace.
* use `namespace NxA {` with the curly bracket on the same line as the namespace name.
* Code defined inside a namespace scope should not be indented.
* `using namespace ...` should always be used for the `NxA` namespace, can be used for any namespaces children of `NxA` and should be avoided for other namespaces like `std`. This helps visually separate code that comes from system or external libraries.
* `local aliases` are fine inside a method as long as it really is local to that method and the aliase is not repeated all over the file.

```
    // --- Like this
    namespace test {

    class MyClass {
        ...
    }

    }

    // --- NOT like this
    namespace test {

        class MyClass {
            ...
        }

    }
```


### Operators

* **Avoid overloading operators** unless the the type is a numeric one and the operators are the basic numeric ones. More often than not overloading seems clever but affect readability if the assumption made by the reader on how the operator works is incorrect. Overloading operators when part of implementing unwrapped standard library api/protocols is allowed.
* **Don't use C++ stream operators** `<<` or `>>` in the code. This is a highly unreadable (right to left) and produces a syntax that looks nothing like no other C++ code. Here too, implementing those operators as part of an implementation of unwrapped standard library api/protocols is allowed.

### Methods

* **Methods should be small**, have ideally 2-3 arguments at the most, do only one thing and have no hidden side effects.
* **Pass in-only-arguments by const reference when possible** and the type is never larger than 3 pointers; otherwise pass by value.
* **Pass out-arguments by non-const ref**.
* **For movable types always provide a non-const move constructor
and move-assignment operator**.
* **Use const** everywhere it is warranted, both for arguments and methods.
* **Avoid passing external vendor types/classes** as arguments outside of a class or method unless absolutely necessary. Any such types should be wrapped into our own types. Standard library types are ok to use as long as they are not available in a wrapped for in `NxA::Base` (for example use `NxA::Array` instead of `std::vector`). Consider extending `NxA::Base` if it does not have all of the required features from the external library. `std` or any other externals should be wrapped. Use/exposure of standard concepts/types like `Iterator begin()` is allowed as this would require way to much cumbersome re-implementing in order to provide the same level of convenience/functionality.

### Memory Management & Pointers

* **Do not use raw pointers** or allocate your own memory unless absolutely necessary. If you need to do this in order to use legacy code, make sure you use the `NXA_SCOPE_EXIT` macro in order to free the memory automatically.
- **If you need to store or pass a raw pointer** make sure you use the `NotNull` class to wrap it and catch null pointers. Pointers should never be passed around as potentially null.
* **Try no to use global data.** This can cause issues with multi-threading and is a good rule even if the code is not multi-threaded. It could be later on.
* **Do not use new/delete.** Creating new objects should be done via a factory method which returns a value or via a constructor called by something like `Shared<>()` or `Unique<>()`.
* **Always use `this->`** for accessing instance variables and instance methods whenever possible. This make it clear that you are accessing a member of the class and not some local method or variable.

### Miscellaneous

* **Always use the types in types.h** instead of built in types. If not available, consider adding the type in `types.h` instead of using the built-in type in our code.
* **Always use pre-increment operator instead of the post increment, when possible.** Pre-increment is often [faster](http://blog2.emptycrate.com/content/why-i-faster-i-c) than post-increment. This is usually optimised nowadays but it's a good habit to retain if the chronology of the increment does not matter.
* **Boolean tests do not test against `true` or `false`** and should just use `!` or nothing.

```
    // --- Like this
    if (!correct) {
        ...
    }
    else if (finished) {
        ...
    }

    // --- NOT like this
    if (correct == false) {
        ...
    }
    else if (finished == true) {
        ...
    }
```

### Comments

* **Avoid using comments** (see "Comments" in [MAR09] page 53). Comments are just lies waiting to happen. Comments can be used for macro documentation but definitely not for explaining step by step how code works.
* **Let the code be the comments** by using descriptive names and break up code into sub-methods instead.

```
    // --- Like this
    decimal timeInSeconds = 0.0f;

    if (!foundItems) {
        parseItemsInList(list);

        time = time + 1.0f;
        ...
    }

    // --- NOT like this
    // -- Time in seconds
    decimal time = 0.0f;

    // -- If we found some items
    if (!found) {
        integer c = 0;

        // -- We go thru every item and parse it
        for (auto&& i : list) {
            i->parse();
        }

        // -- We increment the time
        time = time + 1.0f;
        ...
    }
```

* **TODO comments are ok**. They can even be committed to source control.

```
    // -- TODO: Optimise this.
```

* **Use comments only to explain context.** When something is non-obvious or might not seem logical at first sight.

```
    TagLib::RIFF::WAV::File file(this->filePath->asUTF8(),
                                 true,
                                 TagLib::AudioProperties::ReadStyle::Fast);
    ...

    // -- This is misleading. It doesn't actually save anything to disk.
    // -- Instead, real saving takes place in the file's destructor. #ugh
    file.save();
```

# Classes

* **Every type should have well defined policy for their creation, initialization, moving, and copying**. Every type should have a well-defined set of constructors and destructors (even if they are just deleted).
* **Do not declare public instance variables in classes.** Those should be handled via accessor methods (getters, setters) to the private/protected instance variables contained in the class.
* **Use the rule of 5 defaults**. If your class absolutely requires custom destructors, copy/move constructors or copy/move assignment operators then the class should deal exclusively with ownership and define all of those itself. Other classes should have only default destructors, copy/move constructors or copy/move assignment operators(see [A Concern about the Rule of Zero] (http://scottmeyers.blogspot.ca/2014/03/a-concern-about-rule-of-zero.html)).
* **A base class destructor should be either public and virtual**, or protected and non virtual.  But if you have two types that share implementation but have completely separate resources then declaring a public non-virtual destructor is allowed.
* **Make your classes orthogonal:** as much as possible. Modifying one system or class should not affect another. This is where encapsulation and interface design are crucial. (see "Orthogonality" in [HUN99] page 34). This, of course, is contradicted by the use of a common `NxA::Object::Internal` for value type classes but that should be, as much as possible, the only hard reliance every object has on one another, especially when dealing with high level code.
* **Minimise coupling between modules:** as much as possible. Any method of an object should call only methods belonging to itself, any parameters that were passed in to the method, any objects it created or any directly help component objects (see "The Law of Demeter for Functions" in [HUN99] page 140). Generic arguments for calling is allowed.

```
    template <typename T> class MyClass {
        foo()
        {
            return T::something();
        }
    };
```

# Modern C++ support

* **Use constexpr or typed enums** for constants. Macros should not be used in favour of these tools.
* **Use the new iteration form** for `for()` loops instead of using iterators or indexes.

```
    for(auto&& subTag : subTags) {
        ...
    }
```

* **Initialize member variables with the member initializer list** and not in the constructor. Initializer lists are significantly more efficient; reducing object copies and resizing of containers.

```
    // --- Like this
    MyClass::MyClass() : firstIndex(0)
    {
        ...
    }

    // --- NOT like this
    MyClass::MyClass()
    {
         this->firstIndex = 0;
    }
```

* **You can also can give instance variables a default default value** in the class declaration to avoid having to set it in every constructor.

```
    struct Item : public Object {
        // -- Instance Variables
        integer count = 0;

        ...
    };
```

* **Enable move operations.** Move operations allow the compiler to avoid extra copies by moving temporary objects instead of copying them in certain cases. Certain coding choices we make (such as declaring our own destructor or assignment operator or copy constructor) prevents the compiler from generating a move constructor. Make types MoveAssignable and MoveConstructable where possible and reasonable, avoid making types needlessly movable (i.e., where a copy and a move are equivalent). All types that are movable must also be copyable; unless copy semantics interfere with the purpose of the type (i.e., moving RAII instances).
* **If you use temporary objects** make sure they are casted with `std::move` or `std::forward` correctly otherwise they can prevent the compiler from using a move operator.
* **Use `auto`.** Be sure to use auto with correct cv qualifiers and also with a mind towards universal references, which are often desired.

# Error Handling

* **Use `nothrow`** to indicate if your method does not throw.
* **Do not use return values as error codes.** Use `Optional<>` to wrap a return value that was not correctly produced instead. Return values should always return valid results and never be used to detect an error in the method that was called.
* **Avoid throwing or using exceptions.** This makes error handling more complicated in many ways and the code harder to write and parse. One exception to this is of course assertions which do use `AssertionFailed` in order to completely exit the code safely, allowing it to be caught on the outskirt of the app currently running in order for it to shut down gracefully if need be.

# Unit Testing

* **Test your software** or your users will.
* **Design to test.**
* While **full blown TDD** is not strictly enforced or required, 100% coverage should be aimed for, especially for all low level classes.
* **Don't comment out unit tests when they fail.** Fix the code.
* **Tests should be placed in a Tests subdirectory** of the classes the tests relate to.
* **Use stubs and mocks** to make sure the tests only exercise the part of the code you want them to.

# Source Control & Versioning

* **Always commit one change at a time.** Don't do huge mega commits with tons of different changes in them. If, while developing something, you find the need for some other new functionality then stash your changes, commit the new functionality and pick back up the original change afterward.
* **Do not commit code with commented out sections**, or code that doesn't build or code that doesn't pass all the unit tests.
* **Be careful when committing files that are part of a git subtree**. The commit message will be visible in the commit list of the subtree's origin repo which may be a public repo.
* **Use [git-flow](http://nvie.com/posts/a-successful-git-branching-model/)**. Many git clients support it natively.
* **Always work in a local branch** and not in the develop branch directly.
![](Resources/git-model@2x.png)
* **Use version numbers** that are in the form x.y.z(b) where x is the main version number (1-~), y is the feature number (0-~), z is the hot fix number (0-~) and b is the build number.
* **Increase the feature number** if the change contains new features with or without bug fixes.
* **Increase the hot fix number** if the change only contains bug fixes.
* **Increase the build number** at every new user-facing build.
* **Unless you have direct push access** you should push your changes and commits via a pull request. Pull requests should only contain commits related to one task and only one task.

# Development Rules

* **Use tracer bullets.** When developing new systems, and especially collections of interconnected systems that depend on each other, try to get a dumbed-down stubbed version of the overall system working right away. Things can be hardcoded to begin with, a lot of functionality could be missing but aim to get a working system as quickly as possible. (see "Tracer Bullets" in [HUN99] page 48).
* **Try to keep existing code running at all times.** Don't embark on a huge task that will break your local codebase for days. Try to break up the task into smaller ones that can each be implemented while keeping the code, and the unit tests, working.

# Compilation

* **All compiler warnings or static analyser warnings** should be treated as errors.

# References

This is a list of references used throughout this document, in alphabetical order:

[MEY05] __"Effective C++ Third Edition" by Scott Meyers__ (ISBN 978-0-321-33487-9, [Safari](https://www.safaribooksonline.com/library/view/effective-c-55/0321334876/), [Amazon](http://www.amazon.com/Effective-Specific-Improve-Programs-Designs/dp/0321334876/ref=sr_1_1?ie=UTF8&qid=1431361209&sr=8-1&keywords=9780321334879))

[MAR09] __"Clean Code" by Robert C. Martin.__ (ISBN 978-013-235088-4, [Safari](https://www.safaribooksonline.com/library/view/clean-code/9780136083238/), [Amazon](http://www.amazon.com/Clean-Code-Handbook-Software-Craftsmanship/dp/0132350882/ref=sr_1_1?ie=UTF8&qid=1431316862&sr=8-1&keywords=9780132350884))

[MEY14] __"Effective Modern C++" by Scott Meyers__ (ISBN 978-1-491-90399-5, [Safari](https://www.safaribooksonline.com/library/view/effective-modern-c/9781491908419/), [Amazon](http://www.amazon.com/Effective-Modern-Specific-Ways-Improve/dp/1491903996/ref=sr_1_1?ie=UTF8&qid=1431361448&sr=8-1&keywords=9781491903995))

[HUN99] __"The Pragmatic Programmer" by Andrew Hunt & David Thomas__ (ISBN 978-0-201-61622-4, [Safari](https://www.safaribooksonline.com/library/view/the-pragmatic-programmer/020161622X/), [Amazon](http://www.amazon.com/Pragmatic-Programmer-Journeyman-Master/dp/020161622X/ref=sr_1_1?s=books&ie=UTF8&qid=1458411634&sr=1-1&keywords=the+pragmatic+programmer))
