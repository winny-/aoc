#!/usr/bin/env bash
# This was... rather fun =)
# set -x
required='byr iyr eyr hgt hcl ecl pid'
valid1=0
valid2=0
declare -A record
done=false
until $done; do
    read -r || done=true
    if [[ -z $REPLY ]]; then
        for i in $required; do
            has_value="${record[$i]:+YAY}"
            [[ -z $has_value ]] && break
        done
        if [[ $has_value ]]; then
            (( valid1++ ))
            if [[ ${record[byr]} =~ ^[0-9]{4}$ &&
                  ${record[byr]} -ge 1920 &&
                  ${record[byr]} -le 2002 &&
                  ${record[iyr]} =~ ^[0-9]{4}$ &&
                  ${record[iyr]} -ge 2010 &&
                  ${record[iyr]} -le 2020 &&
                  ${record[eyr]} =~ ^[0-9]{4}$ &&
                  ${record[eyr]} -ge 2020 &&
                  ${record[eyr]} -le 2030 &&
                  ${record[hcl]} =~ ^#[0-9a-f]{6}$ &&
                  ${record[ecl]} =~ ^(amb|blu|brn|gry|grn|hzl|oth)$ &&
                  ${record[pid]} =~ ^[0-9]{9}$ &&
                  ${record[hgt]} =~ ^([0-9]+)(cm|in)$ && (
                      ( ${BASH_REMATCH[2]} = cm &&
                        ${BASH_REMATCH[1]} -ge 150 &&
                        ${BASH_REMATCH[1]} -le 193 ) ||
                      ( ${BASH_REMATCH[2]} = in &&
                        ${BASH_REMATCH[1]} -ge 59 &&
                        ${BASH_REMATCH[1]} -le 76) ) ]]; then
                (( valid2++ ))
            fi
        fi
        unset record
        declare -A record
        continue
    fi
    for item in $REPLY; do
        v=${item#*:}
        k=${item%:*}
        record[$k]=$v
    done
done
echo $valid1
echo $valid2
