import sys
from itertools import chain, tee, filterfalse, permutations
from functools import partial, reduce
import operator


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
    constraints = constraints_list(notes)

    return sum(c
               for t in notes['nearby']
               for c in t
               if not valid_component(constraints, c))


def valid_component(constraints, c):
    return any(lb <= c <= ub for lb, ub in constraints)


def valid_ticket(constraints, ticket):
    return all(valid_component(constraints, c) for c in ticket)


def partition_tickets(constraints, tickets):
    return partition(partial(valid_ticket, constraints), tickets)


def constraints_list(notes):
    return list(chain.from_iterable(notes['constraints'].values()))


def canidates(constraints, components):
    return {con for con in constraints if all(valid_component(con, c) for c in components)}


def part2(notes):
    valid = list(filter(
        partial(valid_ticket, constraints_list(notes)),
        notes['nearby'],
    ))
    components = zip(*notes['tickets'])
    columns = [canidates(can, com) for  can, com in zip(itertools.cycle([set(notes['components'].items())]), components)]
    for idx, column in enumerate(columns):
        if len(column) == 1:
            fixed = next(iter(column))
            for jdx, col in enumerate(columns):
                if jdx == idx:
                    
    while any(len(column) > 1 for column in columns):
        for idx, column in enumerate(columns):
            components
    possible_orders = permutations(notes['constraints'].items())
    for p in possible_orders:
        ok = True
        dbg(p)
        for t in notes['nearby']:
            for (_, constraints), component in zip(p, t):
                if not valid_component(constraints, component):
                    ok = False
                    break
            if not ok:
                break
        if ok:
            return sum(c for idx, ((k, v), c) in enumerate(zip(p, notes['your'])) if k.startswith('departure'))

    return False


if __name__ == '__main__':
    notes = read_notes(sys.stdin)
    print(part1(notes))
    print(part2(notes))

# Local Variables:
# compile-command: "python day16.py < input.txt"
# End:
