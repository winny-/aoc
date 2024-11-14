#!/usr/bin/env perl

use strict;
use warnings;

use Data::Dumper;

# Parse a sequence of numbers into a list.
# " 0 1 2 3 4 5" becomes (0 1 2 3 4 5)
sub parse_numbers {
    my $str = shift;
    $str =~ s/^\s+|\s+$//g;
    my @strings = split(/\s+/, $str);
    my @numbers = map { $_ + 0 } @strings;
    return \@numbers;
}

# Parse a card from a string.
sub parse_card {
    my $str = shift;
    chomp $str;
    my %card;
    if (/Card +(?<id>\d+): +(?<winning>[0-9 ]+) +\| +(?<mine>[0-9 ]+)/) {
        $card{'id'} = $+{id} + 0;
        $card{'winning'} = parse_numbers $+{winning};
        $card{'mine'} = parse_numbers $+{mine};
        $card{'instances'} = 1;
    } else {
        die "Bad card \"${str}\" (regex failed).";
    }
    return %card;
}

sub count_matching {
    my $card = shift;
    my $matches = 0;
    for my $winning (@{$card->{'winning'}}) {
        for my $mine (@{$card->{'mine'}}) {
            if ($winning == $mine) {
                $matches++;
                last;
            }
        }
    }
    return $matches;
}

my %cards;

while (<>) {
    my %card = parse_card($_);
    $cards{$card{'id'}} = \%card;
}

my @ids = sort {$a <=> $b} keys %cards;
my @cards = map {$cards{$_}} @ids;

my $part1 = 0;
for my $card (@cards) {
    if (my $matches = count_matching $card) {
        $part1 += 2**($matches - 1);
    }
}
print "$part1\n";

for my $card (@cards) {
    my $matches = count_matching $card;
    my $id = $card->{id};
    my $instances = $card->{instances};
    # Drain instances
    while ($instances--) {
        # Make copies for each instance.
        for my $won_id ($id+1..$id+$matches) {
            my $won_card = $cards{$won_id};
            $won_card->{instances}++;
        }
    }
}

my $part2 = 0;
for my $card (@cards) {
    $part2 += $card->{instances};
}

print "$part2\n";
