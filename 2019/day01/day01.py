def fuel(mass):
    return max(0, mass // 3 - 2)

def fuel_p2(mass):
    def g():
        f = fuel(mass)
        while f > 0:
            yield f
            f = fuel(f)
    return sum(g())

def part1(L):
    return sum(fuel(mass) for mass in L)

def part2(L):
    return sum(fuel_p2(mass) for mass in L)

if __name__ == '__main__':
    import sys
    L = [int(li) for li in sys.stdin.readlines()]
    print(f'Part #1: {part1(L)}')
    print(f'Part #2: {part2(L)}')
