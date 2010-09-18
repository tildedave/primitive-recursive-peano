#!/bin/sh

cabal configure -ftest

cabal build

dist/build/test/test.exe