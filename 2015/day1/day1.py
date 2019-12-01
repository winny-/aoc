import sys

MAP = {
    '(': 1,
    ')': -1,
}

def calculate_floor(s):
    return sum(MAP.get(c, 0) for c in s)

def main():
    print(calculate_floor(sys.stdin.read()))

if __name__ == '__main__':
    main()
