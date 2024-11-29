from collections import deque
import click


def all_unique(iterable):
    list_ = list(iterable)
    for lower, a in enumerate(list_):
        for b in list_[lower+1:]:
            if a == b:
                return False
    return True


def marker(s, n):
    last_n = deque(maxlen=n)
    for pos, c in enumerate(s):
        last_n.append(c)
        if len(last_n) == last_n.maxlen and all_unique(last_n):
            # click.echo(last_n, err=True)
            return pos+1
    return None


@click.command()
@click.argument('datastream_buffer')
def main(datastream_buffer):
    click.echo(marker(datastream_buffer, 4))
    click.echo(marker(datastream_buffer, 14))


if __name__ == '__main__':
    main()
