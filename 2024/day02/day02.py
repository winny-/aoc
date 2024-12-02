import click


def issafe(levels):
    direction = None
    for a, b in zip(levels, levels[1:]):
        if direction is None:
            direction = a < b
        elif (a < b) != direction:
            return False
        distance = abs(a - b)
        if distance < 1 or distance > 3:
            return False
    return True


def issafe2(levels):
    if issafe(levels):
        return True
    for index, _ in enumerate(levels):
        L = list(levels)
        del L[index]
        if issafe(L):
            return True
    return False


@click.command()
@click.argument('input', type=click.File())
def main(input):
    safe = safe2 = 0
    for line in input:
        levels = [int(n) for n in line.split(' ')]
        safe += issafe(levels)
        safe2 += issafe2(levels)
    print(safe)
    print(safe2)


if __name__ == '__main__':
    main()
