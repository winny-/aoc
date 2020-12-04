BEGIN {
    c = 0;                      # ??? Why is this line needed?
    sx[c] = 1; sy[c++] = 1;
    sx[c] = 3; sy[c++] = 1;
    sx[c] = 5; sy[c++] = 1;
    sx[c] = 7; sy[c++] = 1;
    sx[c] = 1; sy[c++] = 2;
}

NR > 1 {
    for (i = 0; i < c; i++) {
        if (((NR-1) % sy[i]) != 0) {
            continue;
        }
        x[i] += sx[i];
        s = substr($0, 1 + (x[i] % length($0)), 1);
        if (s == "#") {
            trees[i]++;
        }
    }
}

END {
    print trees[1];
    product = 1;
    while(c-- > 0) {
        product *= trees[c];
    }
    print product;
}

# Local Variables:
# compile-command: "awk -f day03.awk < input.txt"
# End:
