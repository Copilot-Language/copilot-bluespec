[![Build Status](https://travis-ci.com/Copilot-Language/copilot.svg?branch=master)](https://app.travis-ci.com/github/Copilot-Language/copilot)

# Copilot: a stream DSL
Copilot-Bluespec implements a Bluespec backend for Copilot, producing code that
is suitable for FPGAs.

Copilot is a runtime verification framework written in Haskell. It allows the
user to write programs in a simple but powerful way using a stream-based
approach.

Programs can be interpreted for testing, or translated Bluespec code to be
incorporated in a project, or as a standalone application. The Bluespec backend
ensures us that the output is constant in memory and time, making it suitable
for systems with hard realtime requirements.

## Installation
Copilot-Bluespec can be found on
[Hackage](https://hackage.haskell.org/package/copilot-bluespec). It is
typically only installed as part of the complete Copilot distribution. For
installation instructions, please refer to the [Copilot
website](https://copilot-language.github.io).

The generated Bluespec code requires `bsc` (the Bluespec compiler) in order to
be compiled. `bsc` can be downloaded
[here](https://github.com/B-Lang-org/bsc/releases).

## Further information
For further information, install instructions and documentation, please visit
the Copilot website:
[https://copilot-language.github.io](https://copilot-language.github.io)

There is also an implementation-focused design document
[here](https://raw.githubusercontent.com/Copilot-Language/copilot/master/copilot-bluespec/DESIGN.md).


## License
Copilot is distributed under the BSD-3-Clause license, which can be found
[here](https://raw.githubusercontent.com/Copilot-Language/copilot/master/copilot-bluespec/LICENSE).
