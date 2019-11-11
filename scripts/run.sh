#!/bin/bash
if [ ! -z $1 ]; then
  ./$1
else
  echo "[UWAGA]: Nie podano pliku do uruchomienia!"
fi
