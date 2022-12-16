#!/bin/bash
tag=$1
cores=$2

xrefcheck --include-untracked +RTS -l -s -N$cores -RTS 2>&1 >/dev/null | tail --lines=+4 > "$tag.stats"
mv xrefcheck.eventlog "$tag.eventlog"
