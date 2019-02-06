# Saha Programming Language

>   Currently under early personal experiment alpha version development. No
>   feature or documentation is to be considered stable, correct, optimized, or
>   secure. Current state of code can be summed up as "hacks upon duct tapes
>   upon hacks, with some neat bits randomly showing through". Any help and
>   support appreciated.
>
>   Development is currently slow, as real life is pushing the author's free
>   time to a minimum.

    /// Application entrypoint.
    function main() int
    {
        // Say hello politely
        print("Hello world!");

        // Just return an integer for exit coding
        return 0;
    }

Saha is an interpreted general purpose programming language with the following
basic features acting as the basis for current development:

-   Static typing
-   Functions
-   Objects
-   Behaviors (interfaces)
-   Type parameterization (generics)

Aimed to fill a space in the PHP, Python, Ruby, etc. language market, being
mainly inspired by PHP and Rust.

## Installation

This project has been written on Linux, so you will have to find out ways to
build this on Windows, Mac, and others by yourself. No guarantees this project
will work on those platforms.

Clone this repository and make sure you have a stable Rust toolchain installed.

Build with

    $ cd /path/to/saha/source
    $ cargo build

Binaries should appear inside `target/debug/`.

A testbench tool is also provided, and some "end-to-end" tests are available
under `tests/e2e/`. These can be run to make sure everything works after a
build. To run these tests invoke

    $ ./target/debug/saha_testbench --command ./target/debug/saha_interpreter --test ./tests/e2e

Each passed test outputs a dot (`.`), failed tests output an error.

## Usage

Write a Saha source file (e.g. `main.saha`)

    function main() int
    {
        return 0;
    }

Then invoke the interpreter as follows:

    $ ./target/debug/saha_interpreter /path/to/saha.main

You can try out the files that are available under `samples/` and see how
changing lines of code alter the output and results.

You can pass simple code to the interpreter via STDIN, but these are limited to
single-file programs:

    $ cat main.saha | ./target/debug/saha_interpreter

## "Todo list"

A short list of stuff that are super broken, somewhat broken,
works-but-in-a-wrong-way, and completely missing:

-   Garbage collection (as in I have not tested how the current symbol table
    value setup works in real life, I presume it is not working properly at all
    and memory and data is leaked like heck)
-   Standard library (super slim, nothing useful available yet)
-   Static analysis of AST (most currently encountered runtime errors should be
    parse errors instead)
-   Documentation (as in how to use this language properly)
-   Testing (as in we need to automatically test the interpreter and tooling
    actually work on a deeper level)
-   Fix the workspace and crates (current separation to subcrates might be
    silly)
-   Extensions (for things like database connections etc. which should be
    outside the scope of the standard library)

Wishlist for upcoming core features:

-   Threading
-   Async execution
-   Package management and installing 3rd party code into a project properly

## Example

Below is an example of what Saha looks like and what it can do on a basic level.

    /// Anything which can be run is Runnable.
    ///
    /// Behaviors are used to define methods for other classes to implement.
    behavior Runnable
    {
        /// Either return an exit code, or a string containing an error message.
        run() Result<int, str>;
    }

    /// A runnable application implementation.
    class Application
    {
        implements Runnable;

        pub prop app_name'str;

        /// Implement the `run()` method from the `Runnable` behavior.
        pub method run() Result<int, str>
        {
            print_line(text = self->getRunMessage());

            var ret'Result<int, str> = new Result<int, str>();

            ret->succeed(value = 0);

            return ret;
        }

        method getRunMessage() str
        {
            return "Hello from " + self->app_name;
        }
    }

    function handle_run_result(result'Result<int, str>) int
    {
        if (result.isSuccess()) {
            return result.unwrap();
        } else {
            print_line(text = result.unwrap());

            return 1;
        }
    }

    /// Program main entrypoint function.
    function main() int
    {
        // No constructors, arguments given to the "newup" are passed directly
        // into the object as properties.
        var app'Runnable = new Application(app_name = "TestApp");

        return handle_run_result(result = app->run());
    }

## Concepts

### Functions, objects, behaviors

Saha code revolves around functions at its core. The absolute unit of
interpretation is a function call, of which a function called `main() int` is
deemed the entrypoint of each program.

```saha
function my_func(str'value)
{
    print(text = value);
}

function main() int
{
    my_func(value = "hello");

    return 0;
}
```

Function call args are keyworded. In case the function only accepts a single
argument, you can omit the keyword as it is inferred by the interpreter.

```saha
function main() int
{
    print(text = "foo");
    print("foo");

    // calls above work identically.

    return 0;
}
```

Saha supports classes and objects, but not inheritance. Classes can implement
behaviors, which provide an interface for other pieces of code to use.

```saha
behavior SaysHello
{
    sayHello();
}

class MyClass
{
    implements SaysHello;

    pub method sayHello()
    {
        print("Hello!");
    }
}
```

Class members are private by default, and can be made public with the `pub`
keyword. Properties which are not defined with a default value must be
initialized when a new class instance is created.

```saha
class HelloClass
{
    prop a'int;

    prop b'int = 2;

    pub prop c'str;
}

function main() int
{
    var hc'HelloClass = new HelloClass(a = 1, c = "foo");

    print(hc->c);

    return 0;
}
```

### Static typing

Saha is statically typed, from local variables to function parameters and return
types. Type checking makes it certain that you do not attempt to operate with
mixed types.

```saha
function hello(int'value) int
{
    print(text = value);

    // above call results in an error, as print expects a `str` and not `int`
    return 1;
}
```

Saha offers the following builtin primary types:

-   `int` for whole numbers
-   `float` for floating point numbers
-   `str` for unicode strings
-   `bool` for boolean values, `true` and `false`

Other types are classes either defined in the standard library or in your own
code. Notable standard library classes are

-   `List<T>` for a list of items
-   `Dict<T>` for a key-value list of items, where keys are `str`
-   `Option<T>` for items that are either `T` or nothing
-   `Result<T, U>` for items that are OK with `T`, or errors with `U`

### Type parameterization/generics

>   Project author has no CS background and so has no idea which term is more
>   correct here.

Saha has generics:

```saha
class MyGenericClass<T>
{
    prop t_prop'T;

    pub method myMethod() T
    {
        return self->t_prop;
    }
}

function main() int
{
    var mgc'MyGenericClass<int> = new MyGenericClass<int>(t_prop = 1);

    var value'int = mgc->myMethod();

    return 0;
}
```

Generics are not limited to builtin types, you can use your own classes and nest
generic code as you wish.

### Errors and recovering

Saha has no concept of exceptions. With the use of `Result<T, U>` type error
management and recovery is forced upon the calling party, who can then decide
whether to handle the error value or pass it on as an error.

```saha
function my_func() : Result<int, str>
{
    var res'Result<int, str> = new Result<int, str>();

    // int is the success type, str is the error type

    return res->succeed(value = 2);
}

function main() int
{
    var result'Result<int, str> = my_func();

    if (result->isOk()) {
        return result->unwrap();
    }

    print(result->unwrap());

    return 1;
}
```

This is very similar to how Rust and Go use result values in error handling.

### Modules and namespacing

Saha supports splitting code into multiple files and directories.

In `/path/to/project/src/main.saha`:

```saha
use pkg.my_module.SubmoduleClass; // `pkg` marks the root of the project which contains the `main.saha` file
use pkg.another.submodule.AnotherClass as AC;

function main() int
{
    var obj'SubmoduleClass = new SubmoduleClass();
    var ac'AC = new AC();

    return obj->getNumber();
}
```

In `/path/to/project/src/my_module.saha`:

```saha
class SubmoduleClass
{
    pub method getNumber() int
    {
        return 4; // guaranteed to be random
    }
}
```

In `/path/to/project/src/another/submodule.saha`:

```saha
class AnotherClass()
{
    //
}
```

### Standard library

> To be written.

## Guides

> To be written.

## Contributing

> To be written.

## License

> To be decided.
