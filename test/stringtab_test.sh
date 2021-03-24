#!/bin/bash

# Create a lot of zero terminated strings, dup them, and 
# feed them to TestStringTab, and make sure we just get
# unique entries back.
ROOT=~/src/gzdoom
find $ROOT -type f -print0 > /tmp/ztest
cat /tmp/ztest /tmp/ztest > /tmp/zfiles.txt

../mod/TestStringTab /tmp/zfiles.txt > /tmp/zout.txt
find $ROOT -type f | sort > /tmp/szfiles.txt
sort /tmp/zout.txt > /tmp/zsort.txt
diff /tmp/szfiles.txt /tmp/zsort.txt
if [ "?" = "0" ]; then
   echo "ERROR mismatch."
   exit 1
fi

