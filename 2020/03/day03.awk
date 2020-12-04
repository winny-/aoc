{
    lines[line_count++] = $0;
}

END {
    slopes[sc++] = "1,1";
    slopes[sc++] = "3,1";
    slopes[sc++] = "5,1";
    slopes[sc++] = "7,1";
    slopes[sc++] = "1,2";

    width = length(lines[0]);

    product = 1;
    
    for (sidx = 0; sidx < sc; sidx++) {
        split(slopes[sidx], delta, ",");
        # printf("%d (%d,%d): ", sidx, delta[1], delta[2]);
        y = 0;
        x_0 = 0;
        while (y < line_count) {
            x_0 += delta[1];
            y += delta[2];
            x = x_0 % width;
            s = substr(lines[y], x + 1, 1);
            if (s == "#") {
                counts[sidx]++;
            }
        }
        # printf("-> %d\n", counts[sidx]);
        product *= counts[sidx];
    }
    print counts[1];
    print product;
}

# Local Variables:
# compile-command: "awk -f day03.awk < input.txt"
# End:
