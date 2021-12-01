#!/usr/bin/env bash

while read -r v; do
    if [[ -n $last && $last -lt $v ]]; then
        (( p1++ ))
    fi
    if [[ ${#window[@]} -eq 3 ]]; then
        if (( window[0]+window[1]+window[2] < window[1]+window[2]+v )); then
            (( p2++ ))
        fi
        window[0]=${window[1]}
        window[1]=${window[2]}
        window[2]=$v
    else
        window+=("$v")
    fi
    last=$v
done
echo "$p1"
echo "$p2"

# Local Variables:
# compile-command: "bash day01.bash < input.txt"
# End:
