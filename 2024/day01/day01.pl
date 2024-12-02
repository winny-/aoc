#!/usr/bin/env perl
use warnings;
use strict;

my (@a1, @a2);

while (<>) {
    if (!/(\d+)\s+(\d+)/) {
        last;
    }
    push @a1, 0+$1;
    push @a2, 0+$2;
}

my (@t1, @t2);
@t1 = sort {$a <=> $b} @a1;
@t2 = sort {$a <=> $b} @a2;

my ($p1, $p2);
for my $i (0 .. $#t1) {
    $p1 += abs($t1[$i] - $t2[$i]);
}
print "$p1\n";

for my $el (@a1) {
    $p2 += $el * grep { $_ == $el } @a2;
}
print "$p2\n";
