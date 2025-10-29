<div align="center">

# TestIt - a test-runner written in Zig

![zig version](https://img.shields.io/badge/zig%20version-0.15-fcca77)
[![Build Status](https://img.shields.io/github/actions/workflow/status/lima1909/testit/ci.yaml?style=for-the-badge)](https://github.com/lima1909/testit/actions)
![License](https://img.shields.io/github/license/lima1909/testit?style=for-the-badge)
[![Stars](https://img.shields.io/github/stars/lima1909/testit?style=for-the-badge)](https://github.com/lima1909/testit/stargazers)

</div>

`TestIt` is a ready-made test-runner AND a library for building your own test-runner, written in ⚡ZIG ⚡.

*TestIt is in a very early stage of development and can change!*


## Features

- Filters tests (run only tests, which you want)
- Shuffles tests (reproducible, by setting a shuffle seed)
- Show slowest tests 
- Show duration of every test
- Memory leak detection
- Multiple output formats: console, JSON (well suited for further processing, like CI or IDE )

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

Run tests (with JSON output):

```bash
❯ zig build test -- --output json 
```

