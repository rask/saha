# Saha Programming Language

>   Currently under early personal experiment alpha version development. No
>   feature or documentation is to be considered stable, correct, or secure.
>   Current state of code can be summed up as "hacks upon duct tapes upon hacks,
>   with some neat bits showing through". Any help and support appreciated.

    /// Application entrypoint.
    function main() int
    {
        // Say hello politely
        print("Hello world!");

        // Just return an integer for exit coding
        return 0;
    }

Saha is a general purpose programming language with the following basic features
acting as the basis for current development:

-   Static typing
-   Functions
-   Objects
-   Behaviors (interfaces for objects)
-   Type parameterization (generics)

Aimed to fill a space in the PHP, Python, Ruby, etc. language market, being
mainly inspired by PHP and Rust.

## Installation

to be written

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

## Guides

to be written

## Contributing

to be written

## License

to be decided
