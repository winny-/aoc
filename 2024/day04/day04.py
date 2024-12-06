import click
from collections import deque
from typing import Optional, List, Iterator
from dataclasses import dataclass


Day04Map = List[str]


@dataclass
class NodeSearch:
    """Node and Search object.  `line`:`column` indicate the location of node
    on the `Day04Map`.  This location contains `symbol`.  If this isn't the
    first `NodeSearch` traversed in this search, `last` contains the previous
    `NodeSearch`.
    """
    line: int
    column: int
    symbol: str
    last: Optional['NodeSearch'] = None

    @property
    def spells(self) -> str:
        L = []
        ns = self
        while True:
            L.append(ns.symbol)
            if ns.last is None:
                break
            ns = ns.last
        L.reverse()
        return ''.join(L)

    @property
    def neighbors(self) -> Iterator[tuple[int, int]]:
        for lo, co in [(-1, -1), (-1, 0), (-1, 1),
                       (0, -1), (0, 1),
                       (1, 1), (1, 0), (1, -1)]:
            yield (self.line+lo, self.column+co)


def desired(ns):
    return 'XMAS'.startswith(ns.spells) or end(ns)


def end(ns):
    return 'XMAS' == ns.spells


@click.command()
@click.argument('input', type=click.File())
@click.option('--debug', type=click.BOOL, is_flag=True)
def main(input, debug):
    part1 = part2 = 0
    L = []

    def valid_linecol(line, col):
        return 0 <= line < len(L) and 0 <= col < len(L[0] if L else [])
    for line in input:
        L.append(line.strip())

    queue = deque()

    for lineno, line in enumerate(L):
        for col, ch in enumerate(line):
            if ch == 'X':
                ns = NodeSearch(line=lineno, column=col, symbol=ch)
                queue.append(ns)

    while queue:
        ns = queue.popleft()
        # if len(ns.spells) > 1:
        #     breakpoint()
        if end(ns):
            part1 += 1
            continue
        if ns.last:
            lo = ns.line - ns.last.line
            co = ns.column - ns.last.column
            line2 = ns.line+lo
            column2 = ns.column+co
            if not valid_linecol(line2, column2):
                continue
            sym = L[line2][column2]
            next_node = NodeSearch(line=line2, column=column2, symbol=sym, last=ns)
            if desired(next_node):
                queue.append(next_node)
        else:
            for (line, column) in ns.neighbors:
                if not valid_linecol(line, column):
                    continue
                sym = L[line][column]
                next_node = NodeSearch(line=line, column=column, symbol=sym,
                                       last=ns)
                if desired(next_node):
                    queue.append(next_node)

    click.echo(part1)

    HL = []
    for lineno, line in enumerate(L):
        for col, ch in enumerate(line):
            if ch != 'A':
                continue
            # if (lineno, col) == (1, 2):
            #     breakpoint()
            # topleft (and NOT topcenter) offsets
            # dr = delta row, dc = delta column.
            dr, dc = -1, -1
            l2, c2 = lineno+dr, col+dc
            if not valid_linecol(l2, c2):
                continue
            sym = L[l2][c2]
            T = {'M': 'S', 'S': 'M'}
            if sym not in T:
                continue
            other = T[sym]
            l3, c3 = lineno-dr, col-dc
            if not valid_linecol(l3, c3):
                continue
            if L[l3][c3] != other:
                continue

            dr2, dc2 = (dc, -dr)
            l4, c4 = lineno+dr2, col+dc2
            if not valid_linecol(l4, c4):
                continue
            sym = L[l4][c4]
            if sym not in T:
                continue
            other = T[sym]
            l5, c5 = lineno-dr2, col-dc2
            if not valid_linecol(l5, c5):
                continue
            if L[l5][c5] != other:
                continue

            HL.extend([(lineno, col),
                       (l2, c2), (l3, c3), (l4, c4), (l5, c5)])

            # click.echo((lineno, col, ch))
            part2 += 1

    if debug:
        for lineno, line in enumerate(L):
            for colno, char in enumerate(line):
                s = click.style(char)
                if (lineno, colno) in HL:
                    if char == 'A':
                        s = click.style(char, fg='black', bg='red')
                    else:
                        s = click.style(char, fg='green', bg='yellow')
                click.echo(s, nl=False)
            click.echo()
    click.echo(part2)


if __name__ == '__main__':
    main()
