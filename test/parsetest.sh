#!/bin/bash

# Exercises the parser and renderer modules by 
# round tripping source through them and verifying
# nothing fails to parse or otherwise gets mangled.
# Assumes all of the executables have already been
# built in the mod directory.
#
# Run from the test directory.
rm -rf testout origtestout
mkdir testout

echo "Parsing and rendering original source."
find ../mod -name "*.mod" | ../mod/TestRender
if [ "$?" != "0" ]; then
   echo "ERROR: TestRender failed with exception?"
   exit 1
fi


echo "Compiling rendered code."
cp ../mod/Makefile testout
pushd testout
make
if [ "$?" != "0" ]; then
   echo "ERROR: Failed building rendered code"
   exit 1
fi
popd

echo "Re-parsing with TestRender built from rendered code."
mv testout origtestout
mkdir testout
find ../mod -name "*.mod" | ../mod/TestRender
if [ "$?" != "0" ]; then
   echo "ERROR: 2nd stage TestRender failed with exception?"
   exit 1
fi

echo "Can we still compile the 2nd pass rendered code?"
cp ../mod/Makefile testout
pushd testout
make
if [ "$?" != "0" ]; then
   echo "ERROR: Failed building 2nd stage rendered code"
   exit 1
fi
popd

# This cheap test may not be valid for all compiler
# toolchains, but it works with bog standard gcc.
cmp testout/TestRender origtestout/TestRender
if [ "$?" != "0" ]; then
   echo "ERROR: executables not the same for rendered code and 2nd stage rendered code."
   exit 1
fi

echo "Render tests done."

