def read(file):

    def parse(line):
        instruction, magnitude = line.split(' ', 1)
        return (instruction, (int(magnitude)))

    return [parse(line) for line in file]


def part12(L):
    hor = dep = 0
    dep2 = aim = 0
    for ins, mag in L:
        match ins:
            case 'forward':
                hor += mag
                dep2 += aim * mag
            case 'down':
                dep += mag
                aim += mag
            case 'up':
                dep -= mag
                aim -= mag
    return (hor * dep, hor * dep2)


if __name__ == '__main__':
    import sys

    L = read(sys.stdin)
    print(*part12(L), sep='\n')


# Local Variables:
# compile-command: "python day02.py < sample.txt"
# End:
