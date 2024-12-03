import click
import re


@click.command()
@click.argument('input', type=click.File())
def main(input):
    do = True
    p1 = p2 = 0
    s = input.read().strip()
    pattern = '|'.join(p.strip() for p in r'''
    (?P<mul>mul)\((?P<n1>\d+),(?P<n2>\d+)\)
    (?P<no>don't)\(\)
    (?P<do>do)\(\)
    '''.split('\n') if p.strip())
    # breakpoint()
    for m in re.finditer(pattern, s):
        if m.group('mul'):
            a = int(m.group('n1'))
            b = int(m.group('n2'))
            product = a * b
            p1 += product
            if do:
                p2 += product
        elif m.group('do'):
            do = True
        elif m.group('no'):
            do = False
        else:
            raise RuntimeError()

    click.echo(p1)
    click.echo(p2)


if __name__ == '__main__':
    main()
