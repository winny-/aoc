import unittest
import re

ENGLISH = dict((y,x) for x,y in enumerate('one two three four five six seven eight nine'.split(), 1))
REGEX = re.compile('|'.join(k for k in ENGLISH.keys()))

def calibration_value_p1(s: str) -> int:
    L = [c for c in s if c.isdigit()]
    return int(L[0] + L[-1])


def part1(xs: list[str]) -> int:
    return sum(calibration_value_p1(x) for x in xs)


def calibration_value_p2(s: str) -> int:
    while True:
        match re.search(REGEX, s):
            case None:
                break
            case re.Match() as m:
                word = m.group(0)
                s = s.replace(word, str(ENGLISH[word]), 1)
    return calibration_value_p1(s)


def part2(xs: list[str]) -> int:
    return sum(calibration_value_p2(x) for x in xs)


class TestDay1(unittest.TestCase):

    sample2_calibration_values = [29, 83, 13, 24, 42, 14, 76]

    def setUp(self):
        with open('sample1.txt', 'r') as f:
            self.sample1 = [line.strip() for line in f.readlines() if line.strip()]
        with open('sample2.txt', 'r') as f:
            self.sample2 = [line.strip() for line in f.readlines() if line.strip()]

    def test_part1(self):
        self.assertEqual(part1(self.sample1), 142)

    def test_part2(self):
        self.assertEqual(part2(self.sample2), 281)

    def test_calibration_value_p2(self):
        self.assertEqual([calibration_value_p2(line) for line in self.sample2],
                         self.sample2_calibration_values)

if __name__ == '__main__':
    with open('input.txt') as f:
        lines = [line.strip() for line in f.readlines() if line.strip()]
    print(part1(lines))
    print(part2(lines))
