import click
import re


@click.command()
@click.argument('input', type=click.File())
def main(input):
    L1, L2 = [], []
    for line in input:
        a, b = [int(i) for i in re.split(r'\s+', line) if i]
        L1.append(a)
        L2.append(b)
    print(sum(abs(a - b) for a, b in zip(sorted(L1), sorted(L2))))
    print(sum(a * L2.count(a) for a in L1))


if __name__ == '__main__':
    main()
