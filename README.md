<div align="center">

# TestIt 

![zig version](https://img.shields.io/badge/zig%20version-0.15-fcca77?style=for-the-badge)
[![Build Status](https://img.shields.io/github/actions/workflow/status/lima1909/testit/ci.yaml?style=for-the-badge)](https://github.com/lima1909/testit/actions)
![License](https://img.shields.io/github/license/lima1909/testit?style=for-the-badge)
[![Stars](https://img.shields.io/github/stars/lima1909/testit?style=for-the-badge)](https://github.com/lima1909/testit/stargazers)

</div>

`TestIt` is a ready-made test-runner AND a library for building your own test-runner, written in ⚡ZIG ⚡.


<div align="center">
<strong>TestIt is in a very early stage of development and can change!</strong>
</div>


## Features

- Filters tests (run only tests, which you want)
- Shuffles tests (reproducible, by setting a shuffle seed)
- Show slowest tests 
- Show duration of every test
- Memory leak detection
- Multiple output formats: console, JSON (well suited for further processing, like CI or IDE )
- configure the test-runner per command-line-args and/or environment variables

## Installation

Clone the TestIt git repository:

```bash
git clone --depth 1 https://github.com/lima1909/testit.git
```

Edit the build.zig file in your project:

```zig
const tests = b.addTest(.{
    .name = "testit-examples", // your name
    .root_module = my_module, // your module
    .test_runner = .{ 
        .path = b.path("testit/src/test_runner.zig"), // path to the testit test_runner.zig
        .mode = .simple,
    },
});
```

## Command Line 

ARGS for the command line usage.

You can set args by command line argument or by environment variable or both.
By both, the command line arguments overwrite the environment variable values.

```bash
# commandline arguments
❯ zig build test -- [ARGS]

# run tests with JSON output
❯ zig build test -- --output json 

# environment and commandline arguments
❯ TESTIT_ARGS="--slowest 1 --filter my-filter --shuffle" zig build test -- --output json
```

| Arg                         | Description                                 | 
|-----------------------------|---------------------------------------------|
| `--slowest [usize]`         | show the [value] slowest tests              |
| `--filter [string]`         | run all tests, which contains the [string]  |
| `--shuffle [u64]`           | shuffle tests, which optional seed [u64]    |
| `--output [console|json]`   | output format, default is `console`         |
| `--stderr`                  | output to `stderr`, default is `stdout`     |
