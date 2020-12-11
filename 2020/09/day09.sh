#!/bin/sh
# This is the helper script to assist GNU bc with an arbritrary length of
# input.
set -eu
TMPFILE="$(mktemp -t 'tmp.XXXXXXX')"
cleanup() { rm "$TMPFILE"; }
trap cleanup EXIT
cat > "$TMPFILE"
{
    wc -l < "$TMPFILE";
    cat < "$TMPFILE"
} | bc -l day09.bc
