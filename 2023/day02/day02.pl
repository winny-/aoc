#!/usr/bin/env perl
# https://adventofcode.com/2023/day/2

use strict;
use warnings;

use Data::Dumper;

sub parse_game {
    my $str = shift;
    my %result;
    if ($str =~ /Game (?<id>\d+): (?<rounds>[ ,;0-9a-z]*)/) {
        $result{'id'} = $+{id} + 0;
    } else {
        die "Bad game \"${str}\" (regex failed)";
    }
    my @rounds;
    for my $round (split / *; */, $+{rounds}) {
        my %components;
        for my $component (split / *, */, $round) {
            my ($quantity, $color) = split / +/, $component;
            if (exists $components{$color}) {
                die "Bad game \"${str}\": round: ${round}";
            }
            $components{$color} = $quantity + 0;
        }
        push(@rounds, \%components);
    }

    $result{'rounds'} = \@rounds;
    return %result;
}

sub color {
    my $round = shift;
    my $color = shift;
    return exists $round->{$color} ? $round->{$color} : 0;
}

# https://www.perlmonks.org/?node_id=406887
sub max {
    my ($max, @vars) = @_;
    for (@vars) {
        $max = $_ if $_ > $max;
    }
    return $max;
}

my @games;
while (<>) {
    my %g = parse_game $_;
    push(@games, \%g);
}
# print Dumper(\@games);

my $part1 = 0;
P1_GAME: for my $game (@games) {
    for my $round (@{$game->{'rounds'}}) {
        next P1_GAME if color(\%$round, 'red') > 12;
        next P1_GAME if color(\%$round, 'green') > 13;
        next P1_GAME if color(\%$round, 'blue') > 14;
    }
    my $id = $game->{'id'};
    # print "P1 ${id}\n";
    $part1 += $id;
}

print $part1, "\n";

my $power = 0;

for my $game (@games) {
    my ($red, $blue, $green);
    $red = 0;
    $blue = 0;
    $green = 0;
    for my $round (@{$game->{'rounds'}}) {
        $red = max(color(\%$round, 'red'), $red);
        $blue = max(color(\%$round, 'blue'), $blue);
        $green = max(color(\%$round, 'green'), $green);
    }
    $power += $red * $blue * $green;
}
print "${power}\n";
