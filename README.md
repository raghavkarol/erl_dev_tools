# erl_dev_tools

An Erlang application that watches for changes to monitored
directories and has an API to compile changed files and or run tests.

Why? Running,

```
rebar ct suite=<my_suite> case=<my_case> takes long (seconds)!
```

For projects making testing and test driven development painfully
slow. Most of the time is spent in starting the erlang runtime, so why
not start the runtime once and use it for compiling and testing fast.

Additionally, one can use Emacs+Distel to communicate with this
running node and compile and run tests with an Emacs key bindings.

# Next Steps

Dogfooding.