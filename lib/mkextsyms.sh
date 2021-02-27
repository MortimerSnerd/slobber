#!/bin/bash

# Generate symbol tables external modules
# where we only have stubs, not the source. 

BINDIR=`cd ../mod && pwd`
MKEXE=$BINDIR/MkSymtab

OBNCSRC=~/src/obnc-0.16.1/lib/obnc
OBNCEXTSRC=~/src/obnc-libext-0.7.0/ext

mkSym() {
   local log=/tmp/mksym.log

   echo "$MKEXE $*"
   $MKEXE $* 2>&1 > $log
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

# Build symbols for libraries from obnc we use first.
mkSym $OBNCSRC/Files.obn $OBNCSRC/In.obn $OBNCSRC/Input0.obn
mkSym $OBNCSRC/Input.obn $OBNCSRC/Out.obn $OBNCSRC/Strings.obn
mkSym $OBNCEXTSRC/extArgs.obn $OBNCEXTSRC/extConvert.obn
mkSym $OBNCEXTSRC/extEnv.obn

