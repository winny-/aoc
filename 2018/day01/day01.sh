#!/usr/bin/env bash

set -eu

mapfile inputs

declare -A seen
seen[0]=1
sum=0
first=yes
while :; do
    for n in "${inputs[@]}"; do
        let sum=sum+n
        if [[ -v seen[$sum] ]]; then
            echo "Part 2: $sum"
            exit 0
        else
            seen[$sum]=1
        fi
    done
    if [[ $first = yes ]]; then
        echo "Part 1: $sum"
        first=no
    fi
done
