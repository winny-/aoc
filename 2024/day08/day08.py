import click
from collections import defaultdict
from typing import NamedTuple


class RowCol(NamedTuple):
    row: int
    col: int

    def __add__(self, other):
        if isinstance(other, RowCol):
            return RowCol(self.row + other.row, self.col + other.col)
        raise TypeError(f'Cannot add RowCol to {type(other)}')

    def __sub__(self, other):
        if isinstance(other, RowCol):
            return RowCol(self.row - other.row, self.col - other.col)
        raise TypeError(f'Cannot subtract RowCol from {type(other)}')


@click.command()
@click.argument('input', type=click.File())
@click.option('--debug', is_flag=True)
def main(input, debug):
    map_ = {}
    by_frequency = defaultdict(set)

    for lineno, line in enumerate(input):
        for colno, cell in enumerate(line.strip()):
            rowcol = RowCol(lineno, colno)
            match cell:
                case '.':
                    pass
                case frequency:
                    map_[rowcol] = frequency
                    by_frequency[frequency].add(rowcol)
            width = colno+1
        height = lineno+1

    part1 = part2 = 0  # Initialize the variables to contain the solutions.
    antinodes1 = set()
    antinodes2 = set()

    def printmap(antinodes=None):
        for row in range(height):
            for col in range(width):
                rowcol = RowCol(row, col)
                sym = '.'
                if rowcol in map_:
                    sym = map_[rowcol]
                    if antinodes is not None and rowcol in antinodes:
                        sym = click.style(sym, reverse=True)
                elif antinodes is not None and rowcol in antinodes:
                    sym = '#'
                click.echo(sym, nl=False)
            click.echo()

    for rowcol, frequency in map_.items():
        for other in by_frequency[frequency] - set(rowcol):
            distance = rowcol - other
            for antinode in [rowcol + distance, other - distance]:
                if not (0 <= antinode.row < height and
                        0 <= antinode.col < width):
                    continue
                if map_.get(antinode, None) == frequency:
                    continue
                antinodes1.add(antinode)
            # TODO Part 2

    part1 = len(antinodes1)
    part2 = len(antinodes2)

    if debug:
        printmap(antinodes1)
        click.echo()
        printmap(antinodes2)

    # Print the two solutions out with one per line.
    click.echo(part1)
    click.echo(part2)


if __name__ == '__main__':
    main()
