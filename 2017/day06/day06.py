import sys
import time


def redistribute(banks):
    banks = list(banks)
    index, blocks = max(enumerate(banks),
                        key=lambda a: (a[1], len(banks)-a[0]))
    banks[index] = 0
    for i in range(1, blocks+1):
        banks[(index+i) % len(banks)] += 1
    return banks


if __name__ == '__main__':
    banks = [int(blocks) for blocks in sys.stdin.readline().split('\t')]
    seen = {tuple(banks): 0}
    i = 0
    while True:
        i += 1
        banks = redistribute(banks)
        t = tuple(banks)
        if t in seen:
            print(i)
            print(i-seen[t])
            exit()
        seen[t] = i
        # print(banks, seen, i)
        # time.sleep(.5)

