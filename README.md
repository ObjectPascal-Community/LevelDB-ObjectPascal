# LevelDB for Object Pascal

[![Build Status](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/actions)
[![Supports Windows](https://img.shields.io/badge/support-Windows-blue?logo=Windows)](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/releases/latest)
[![Supports Linux](https://img.shields.io/badge/support-Linux-yellow?logo=Linux)](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/releases/latest)
[![License](https://img.shields.io/github/license/ObjectPascal-Community/LevelDB-ObjectPascal)](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/blob/master/LICENSE)
[![Latest Release](https://img.shields.io/github/v/release/ObjectPascal-Community/LevelDB-ObjectPascal?label=latest%20release)](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/releases/latest)
[![Downloads](https://img.shields.io/github/downloads/ObjectPascal-Community/LevelDB-ObjectPascal/total)](https://github.com/ObjectPascal-Community/LevelDB-ObjectPascal/releases)


> **Disclaimer**
>
> In the spirit of full disclosure and due to my utter laziness in coming up with the bulk of the boilerplate code, I turned to `ChatGPT` and `Claude`.\
> You can see the series of prompts and answers I've got in the [research](research) folder.

This is a `C` header translation for the key-value pair database [`LevelDB`](https://github.com/google/leveldb) from Google.

It also contains a skeleton class to wrap the `C` header translation and make it a bit easier to use.

## Prerequisites

### Linux

The package `libleveldb1d` should be installed in order for the tests to compile:

```console
$ sudo apt install libleveldb1n
```

> **NOTE**: Since this is being tested on an Ubuntu 24.04, I'll need the community to help me pin down any different package.

### Windows

Coming soon.

### macOS

Coming soon.
