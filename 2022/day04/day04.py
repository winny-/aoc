import sys
import re


if __name__ == '__main__':
    part1 = part2 = 0
    for line in sys.stdin:
        al, ah, bl, bh = [int(i) for i in re.split(r"[,-]", line)]
        # Complete overlap
        if (
                al <= bl <= ah or al <= bh <= ah or
                bl <= al <= bh or bl <= ah <= bh):
            part2 += 1
            if al <= bl <= bh <= ah or bl <= al <= ah <= bh:
                part1 += 1

    print(part1, part2, sep='\n')
