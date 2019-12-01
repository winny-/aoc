BEGIN {
    sum=0
}

{
    sum = sum + $1
}

END {
    print sum
}
