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
MKEXE=$BINDIR/MkSymtab

# We reference some libraries that come with OBNC, so we'll need
# to make symbols for those as well. We need the obnc source
# directories, the install dirs won't do.
OBNCSRC=~/src/obnc-0.16.1/lib/obnc
OBNCEXTSRC=~/src/obnc-libext-0.7.0/ext

mkSym() {
   local log=/tmp/mksym.log

   $MKEXE $* 2>&1 | tee $log
   grep -i error $log
   if [ "$?" = "0" ]; then
      echo ""
      echo "ERROR detected."
      exit 1
   fi

   grep -i exception $log
   if [ "$?" = "0" ]; then
      echo ""
      echo "EXCEPTION/ABORT detected."
      exit 1
   fi
}

rm -rf symout
mkdir symout
cd symout

# Build symbols for libraries from obnc we use first.
mkSym $OBNCSRC/Files.obn $OBNCSRC/In.obn $OBNCSRC/Input0.obn
mkSym $OBNCSRC/Input.obn $OBNCSRC/Out.obn $OBNCSRC/Strings.obn
mkSym $OBNCEXTSRC/extArgs.obn $OBNCEXTSRC/extConvert.obn
mkSym $OBNCEXTSRC/extEnv.obn

# List of files ordered so files that are imported come before
# the files that import them.
FILES="Path.mod Dbg.mod Config.mod BinReader.mod BinWriter.mod\
       Scanner.mod Ast.mod Types.mod"

for f in $FILES; do
   mkSym ../../mod/$f
done

