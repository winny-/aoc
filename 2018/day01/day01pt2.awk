BEGIN {
    numbers[0] = 0
    numbersCount=0
}

{
    numbers[numbersCount++] = $1
}

END {
    sum=0
    seen[0] = 0 # is overwritten
    seenIndex=0
    iterIndex=0
    do {
        seen[seenIndex++] = sum
        if (iterIndex>=numbersCount) {
            iterIndex=0
        }
        printf("ok %s %s\n", sum, seenIndex)
        sum = sum + numbers[iterIndex++]
    } while (!(sum in seen))
    print sum
}
