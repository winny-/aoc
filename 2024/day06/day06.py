import click
from typing import TextIO, Set, Dict, NamedTuple


DIR = {'^': (-1, 0),
       'v': (1, 0),
       '<': (0, -1),
       '>': (0, 1)}


Point = tuple[int, int]
Direction = tuple[int, int]
Turn = tuple[Direction, Point]
Puzzle = Dict[Point, str]


class TraceResult(NamedTuple):
    loop: bool
    turns: Set[Turn]
    visited: Set[Point]


def show(puzzle: Puzzle,
         wxh: tuple[int, int],
         visited: Set[Point] = None,
         pos: Point = None) -> None:
    width, height = wxh
    for line in range(height):
        for col in range(width):
            rowcol = (line, col)
            if ch := puzzle.get(rowcol):
                click.echo(ch, nl=False)
            elif pos == rowcol:
                click.secho('@', reverse=True, nl=False)
            elif visited is not None and rowcol in visited:
                click.echo('X', nl=False)
            else:
                click.echo('.', nl=False)
        click.echo()


def trace(start: Turn,
          puzzle: Dict[Point, str],
          wxh: tuple[int, int]) -> TraceResult:
    direction, pos = start
    turns: Set[Turn] = set()
    visited: Set[Point] = set([pos])
    width, height = wxh
    loop = False

    def bounded(rowcol):
        r, c = rowcol
        return 0 <= r < width and 0 <= c < height

    def valid(rowcol):
        r, c = rowcol
        return bounded(rowcol) and puzzle.get(rowcol, '.') not in '#O'

    while True:
        next_pos = pos[0] + direction[0], pos[1] + direction[1]
        if not bounded(next_pos):
            break
        elif not valid(next_pos):
            turn = (direction, pos)
            if turn in turns:
                loop = True
                break
            direction = (direction[1], -direction[0])
            turns.add(turn)
            continue
        pos = next_pos
        visited.add(pos)

    return TraceResult(loop, turns, visited)


@click.command()
@click.argument('input_', metavar='input.txt', type=click.File())
def main(input_: TextIO) -> None:
    pos = None
    direction = None
    puzzle = {}
    for lineno, line in enumerate(input_):
        for colno, cell in enumerate(line.strip()):
            match cell:
                case '#' | 'O':
                    puzzle[(lineno, colno)] = cell
                case _ if cell in DIR:
                    direction = DIR[cell]
                    pos = (lineno, colno)
                case x if x != '.':
                    raise ValueError(f'Unhandled case for \"{x}\"')
    width = max(r for r, _ in puzzle.keys())+1
    height = max(c for _, c in puzzle.keys())+1
    if direction is None:
        raise ValueError('Could not find starting direction!')
    if pos is None:
        raise ValueError('Could not locate starting point!')

    start = (direction, pos)
    wxh = (width, height)
    res = trace(start, puzzle, wxh)
    click.echo(len(res.visited))

    part2 = 0
    for pos in res.visited - set([pos]):
        res = trace(start, puzzle.copy() | {pos: 'O'},  wxh)
        if res.loop:
            part2 += 1
    click.echo(part2)


if __name__ == '__main__':
    main()
