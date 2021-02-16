#!/bin/bash
# Builds symbols for the stub files in this directory.
# Sym files are left in this directory.
rm -f *.slo
../../mod/MkSymtab *.mod
mv SYSTEM.slo ..
