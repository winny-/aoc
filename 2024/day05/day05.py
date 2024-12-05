import click
from collections import defaultdict
import re


@click.command()
@click.argument('input', type=click.File())
def main(input):
    rules = defaultdict(list)
    updates = []
    part1 = part2 = 0
    for line in input:
        if m := re.match(r'(\d+)\|(\d+)', line):
            rules[int(m.group(1))].append(int(m.group(2)))
        elif strings := re.findall(r'\d+', line):
            updates.append([int(s) for s in strings])

    for update in updates:
        for index, page in enumerate(update):
            intersection = set(rules.get(page, [])) & set(update[:index])
            if intersection:
                break
        else:
            part1 += update[len(update)//2]
    click.echo(part1)
    # click.echo(rules)
    # click.echo(updates)


if __name__ == '__main__':
    main()
