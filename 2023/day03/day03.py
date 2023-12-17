from collections.abc import Iterable
import unittest, re

NUMBER_REGEX = re.compile(r'[0-9]+')
NUMBER_BEGIN_REGEX = re.compile(r'^[0-9]+')
NUMBER_END_REGEX = re.compile(r'[0-9]+$')
SYMBOL_REGEX = re.compile(r'[^0-9.]')
GEAR_REGEX = re.compile(r'\*')


def part_numbers(lines: [str]) -> Iterable[int]:
    for lno, line in enumerate(lines):
        for m in NUMBER_REGEX.finditer(line):
            start, end = m.span(0)
            prev_lno, next_lno = lno-1, lno+1
            def check_line(target_line):
                return (0 <= target_line < len(lines)
                        and SYMBOL_REGEX.search(lines[target_line][max(0, start-1):end+1]))
            if check_line(prev_lno) or check_line(next_lno) or check_line(lno):
                i = int(m.group(0))
                yield i

def interested_lines(lines: [str], lino: int) -> Iterable[str]:
    plino = lino-1
    if plino >= 0:
        yield lines[plino]
    yield lines[lino]
    nlino = lino+1
    if nlino < len(lines):
        yield lines[nlino]


def neighbors(lines: [str], lino: int, col: int) -> Iterable[int]:
    for line in interested_lines(lines, lino):
        for m in NUMBER_REGEX.finditer(line):
            (start, end) = m.span(0)
            if ((col <= end <= col+1) or
                (col-1 <= start <= col+1) or
                (start <= col <= end)):
                yield int(m.group(0))


def gear_ratios(lines: [str]) -> Iterable[int]:
    for lno, line in enumerate(lines):
        for m in GEAR_REGEX.finditer(line):
            match list(neighbors(lines, lno, m.start(0))):
                case [x, y]:
                    yield x*y


def part1(lines: [str]) -> int:
    return sum(part_numbers(lines))


def part2(lines: [str]) -> int:
    return sum(gear_ratios(lines))


class TestDay03(unittest.TestCase):

    def setUp(self):
        with open('sample.txt') as f:
            self.sample = f.readlines()

    def test_part1_with_sample(self):
        self.assertEqual(part1(self.sample), 4361)

    def test_part2_with_sample(self):
        self.assertEqual(part2(self.sample), 467835)


if __name__ == '__main__':
    with open('input.txt') as f:
        lines = f.readlines()
        print(part1(lines))
        print(part2(lines))
