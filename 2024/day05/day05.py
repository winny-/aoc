import click
from collections import defaultdict
from dataclasses import dataclass
from typing import Any, Set, Dict, TextIO
import re


@dataclass
class InvalidOrder:
    page: int
    index: int
    intersection: Set[int]


@dataclass
class Result:
    """Strategy for returning values alongside a success/failure flag.  The
    entire object's truthiness is indicated by `success`.  Use `value` to
    communicate any data regarding the operation's success/failure.

    Example:

        def tweet(msg):
            ...
            if tweet_successful:
                return Result(True, tweet_id)
            else:
                return Result(False, errmsg)

        if ok := tweet("hi python community!"):
            print(f"Tweet ID: {ok.value}")
        else:
            print(f"Failure: {ok.value}")
    """
    success: bool
    value: Any

    def __bool__(self):
        return self.success


@click.command()
@click.argument('input', type=click.File())
def main(input: TextIO) -> None:
    rules: Dict[int, Set[int]] = defaultdict(set)

    def correctly_ordered(update) -> Result:
        for index, page in enumerate(update):
            intersection = rules[page] & set(update[:index])
            if intersection:
                invalid_order = InvalidOrder(
                    page=page,
                    intersection=intersection,
                    index=index,
                )
                return Result(False, invalid_order)
        return Result(True, None)

    updates = []
    part1 = part2 = 0
    for line in input:
        if m := re.match(r'(\d+)\|(\d+)', line):
            rules[int(m.group(1))].add(int(m.group(2)))
        elif strings := re.findall(r'\d+', line):
            updates.append([int(s) for s in strings])

    for update in updates:
        update2 = list(update)
        first = True
        while not (ok := correctly_ordered(update2)):
            first = False
            del update2[ok.value.index]
            new_index = max(update2.index(dep)
                            for dep in ok.value.intersection)
            update2.insert(new_index, ok.value.page)
        middle = update2[len(update2)//2]
        if first:
            part1 += middle
        else:
            part2 += middle
    click.echo(part1)
    click.echo(part2)


if __name__ == '__main__':
    main()
