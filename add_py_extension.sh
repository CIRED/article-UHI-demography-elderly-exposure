#!/bin/bash

for file in *; do
  echo "Checking: $file"
  if [[ -f "$file" && "$file" != *.* ]]; then
    echo "Renaming: $file -> $file.py"
    mv -- "$file" "$file.py"
  fi
done
