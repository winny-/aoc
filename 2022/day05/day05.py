from io import StringIO
import click
import re
from dataclasses import dataclass
from collections import defaultdict, deque
from copy import deepcopy


DEBUG = False


@dataclass
class Instruction:
    count: int
    src: str
    dest: str

    def __str__(self):
        return f'move {self.count} from {self.src} to {self.dest}'


def stacks_str(stacks):
    keys = list(sorted(stacks.keys()))
    with StringIO() as io:
        maxlen = max(len(stack) for stack in stacks.values())
        for offset in range(maxlen):
            for k in keys:
                stack = stacks[k]
                addr = maxlen - offset - 1
                if len(stack) <= addr:
                    print('   ', end=' ', file=io)
                else:
                    print(f'[{stack[-addr-1]}]', end=' ', file=io)
            print(file=io)
        for k in keys:
            print(f' {k} ', end=' ', file=io)
        print(file=io)
        return io.getvalue()


def load_stacks(fileo):
    stacks = defaultdict(deque)
    for line in fileo:
        if not line.strip():
            raise RuntimeError('Should not reach')
        for index, row in enumerate(re.finditer(r'\[([A-Z])]|( ){3} ?', line)):
            if (g1 := row.group(1)) and (value := g1.strip()):
                stacks[index].append(value)
        if m := re.match(r'^ *([0-9]+[ 0-9]*[0-9]+) *$', line):
            labels = re.split(r' +', m.group(1))
            for index, label in enumerate(labels):
                stacks[label] = stacks[index]
                del stacks[index]
            return dict(stacks)  # Shed autovivification
    raise RuntimeError('Should not reach')


def load_instructions(fileo):
    instructions = []
    for line in fileo:
        if not line.strip():
            pass
        elif m := re.match('move ([0-9]+) from ([0-9]+) to ([0-9]+)', line):
            countstr, src, dest = m.groups()
            instructions.append(Instruction(int(countstr), src, dest))
        # ???
    return instructions


def simulate(stacks, instructions, core):
    for instruction in instructions:
        src, dest = stacks[instruction.src], stacks[instruction.dest]
        core(instruction=instruction, src=src, dest=dest)
    L = []
    for s in sorted(stacks.items(), key=lambda x: x[0]):
        L.append(s[1][0])
    return ''.join(L)


@click.command()
@click.argument('input_file', type=click.File(), default='input.txt')
def main(input_file):
    stacks = load_stacks(input_file)
    instructions = load_instructions(input_file)
    if DEBUG:
        print(stacks_str(stacks))
        for instruction in instructions:
            print(instruction)

    stacks2 = deepcopy(stacks)

    def part1_core(instruction, src, dest):
        for i in range(instruction.count):
            dest.appendleft(src.popleft())
    print(simulate(stacks, instructions, part1_core))

    def part2_core(instruction, src, dest):
        for box in reversed([src.popleft() for _ in range(instruction.count)]):
            dest.appendleft(box)
    print(simulate(stacks2, instructions, part2_core))


if __name__ == '__main__':
    main()
