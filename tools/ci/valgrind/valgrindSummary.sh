#!/bin/bash

ver="<unknown>"
hook="${SLACK_WEBHOOK_SECRET}"

if [ $# -ge 1 ]; then
     ver=$1
     #echo "Version is ${ver}."
fi

## check for successful, or unsuccessful, output and use either
for suffix in Rout Rout.fail; do
    file="tiledb.Rcheck/tests/tinytest.${suffix}"
    if test -f ${file}; then
        echo ""
        echo "** Using ${suffix}"
        echo ""
        grep "^==" ${file}

        ## pipe log through tr(1) to translate "345,678" (for humans) into "345678"
        ## then use an awk script to pick the three figures of interest and, if
        ## their sum is non-negative, emit a summary string
        lostsummary=$(cat ${file} | tr -d [,] | awk -f tools/ci/valgrind/sumLosses.awk )

        if [ ${lostsummary} != "" ] && [ ${hook} != "" ]; then
            curl -X POST -H 'Content-type: application/json' --data "{\"text\":\"Running *$ver*:\n $lostsummary\"}" https://hooks.slack.com/services/"${hook}"
            echo ""
            echo "Message '${ver}' : '${lostsummary}' sent"
            echo ""
        else
            echo ""
            echo "No losses. Good. Nothing to send."
            echo ""
        fi
    fi
done

echo "Done."
