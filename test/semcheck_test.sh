#!/bin/bash

# Run semcheck on all project source files.
# MkSymtab doesn't chase dependencies, so we need
# to build the deps leaf downwards ourselves.
#
# This script assumes that the symbols in lib/stubs
# are already built.  We don't build those automatically
# as updating those at the wrong time could cause 
# bootstrapping problems.

BINDIR=`cd ../mod && pwd`
MKEXE=$BINDIR/Compile

# We reference some libraries that come with OBNC, so we'll need
# to make symbols for those as well. We need the obnc source
# directories, the install dirs won't do.
OBNCSRC=~/src/obnc-0.16.1/lib/obnc
OBNCEXTSRC=~/src/obnc-libext-0.7.0/ext

mkSym() {
   local log=/tmp/mksym.log

   echo "$MKEXE $*"
   $MKEXE $* 2>&1 |tee $log
   if [ "$?" != "0" ]; then
      echo ""
      cat $log
      echo "ERROR: Crash?"
      exit 1
   fi
   grep -i error $log
   if [ "$?" = "0" ]; then
      echo ""
      cat $log
      echo "ERROR detected."
      exit 1
   fi

   grep -i exception $log
   if [ "$?" = "0" ]; then
      echo ""
      cat $log
      echo "EXCEPTION/ABORT detected."
      exit 1
   fi
}

rm -rf symout
mkdir symout
cd symout

# List of main entry points.  We use the compiler to generate
# just the symbols. (which forces semchecking).
ROOTS="TestScanner.mod TestParser.mod TestRender.mod TestSymtab.mod\
       MkSymtab.mod DumpAst.mod Compile.mod"

for f in $ROOTS; do
   mkSym -s ../../mod ../../mod/$f
done

