#!/bin/bash

## check for successful, or unsuccessful, output and show either
for suffix in Rout Rout.fail; do
    if test -f tiledb.Rcheck/tests/tinytest.${suffix}; then
        echo ""
        echo "** Using ${suffix}"
        echo ""
        grep "^==" tiledb.Rcheck/tests/tinytest.${suffix}
    fi
done

echo "Done."
