import sys


def dbg(x):
    # print(str(x), repr(x), type(x), sep=' :: ')
    return x


def read_notes(f):
    """This is the wrong way to read a file, but it does work."""
    constraints = {}
    your = None
    nearby = []
    state = 0
    for line in f:
        line = line.strip('\r\n')
        # print('line: ', line, sep='')
        if not line:
            # print('not line')
            state += 1
            if state > 2:
                break
            continue
        if state == 0:
            field_name, v = line.split(': ', 1)
            L = [z for z in v.split(' or ')]
            L2 = [dbg(o.split('-')) for o in L]
            field_constraints = [(int(a), int(b)) for (a, b) in L2]
            constraints[field_name] = field_constraints
        elif state == 1:
            if 'your ticket' in line:
                # print('yt')
                continue
            your = parse_ticket(line)
        else:  # 2
            if 'nearby tickets' in line:
                # print('near')
                continue
            nearby.append(parse_ticket(line))
    return dbg({
        'constraints': constraints,
        'your': your,
        'nearby': nearby,
    })


def parse_ticket(s):
    return [int(ss) for ss in s.split(',')]


def part1(notes):
    constraints = []
    for field_constraint in notes['constraints'].values():
        constraints.extend(field_constraint)

    def valid_component(c):
        for (lb, ub) in constraints:
            if lb <= c <= ub:
                return True
        return False

    def ticket_errors(t):
        return [c for c in t if not valid_component(c)]

    return sum(sum(ticket_errors(t)) for t in notes['nearby'])


if __name__ == '__main__':
    notes = read_notes(sys.stdin)
    print(part1(notes))
