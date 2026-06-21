#!/usr/bin/env sh

$1 --optimize-level $2 --program ./test.ss \
    && echo "====== OPT LEVEL $2 OK ======" \
            || echo "====== OPT LEVEL $2 NOT OK!!! ======"
