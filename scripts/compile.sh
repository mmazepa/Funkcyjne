#!/bin/bash
if [ ! -z $1 ]; then
  ghc -o $1 "$1".hs
  rm *.hi
  rm *.o
else
  echo "[UWAGA]: Nie podano pliku do kompilacji!"
fi
