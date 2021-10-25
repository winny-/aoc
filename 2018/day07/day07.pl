use strict;

# A fun DAG problem... Let's learn some Perl5!

my (%graph, @available, @finished, @doing, $tick, @starts);

# Read in the input.
while (<> =~ /Step (\w+) must be finished before step (\w+) can begin./) {
    push @{$graph{$1}}, $2;
    # print "$2 -> $1\n";
}

# Find the possible starts in the DAG.
 outer:
    foreach my $vertex (keys %graph) {
        foreach my $edges (values %graph) {
            # print "testing $vertex with $edges\n";
            if (grep $_ eq $vertex, @{$edges}) {
                # my $s = join(", ", @{$edges});
                # print "matched $vertex in $s $edges\n";
                next outer;
            }
        }
        push @starts, $vertex;
}

# Part 1
@available = @starts;
while (@available) {
    @available = sort @available;
    my $current = shift @available;
    push @finished , $current;
  o2:
    # Follow each immediate vertex.
    foreach my $vertex (@{$graph{$current}}) {
        keys %graph;
        while (my ($v2, $e2) = each %graph) {
            # Filter out vertices that cannot be visited yet, or that have
            # already been visited.
            if (grep($_ eq $vertex, @{$e2}) && !grep($_ eq $v2, @finished)) {
                next o2;
            }
        }
        push @available, $vertex;
  }
}

print join('', @finished), "\n";


@finished = ();
@available = @starts;
while (@available || @doing) {
    @available = sort @available;
    while (scalar @doing < 5 && @available) {
        my $task = shift @available;
        my @ary = ($task, 0);
        push @doing, \@ary;
    }
    my (@done, @todo);
    @done = ();
    @todo = ();
    foreach my $worker (@doing) {
        if (++@{$worker}[1] >= 61+ord(@{$worker}[0])-ord('A')) {
            push @done, @{$worker}[0];
        } else {
            push @todo, $worker;
        }
    }
    push @finished, sort @done;
    @doing = @todo;
    # Follow each immediate vertex.
    foreach my $vfin (@done) {
      o2:
        foreach my $vertex (@{$graph{$vfin}}) {
            keys %graph;
            while (my ($v2, $e2) = each %graph) {
                # Filter out vertices that cannot be visited yet, or that have
                # already been visited.
                if (grep($_ eq $vertex, @{$e2}) && !grep($_ eq $v2, @finished)) {
                    next o2;
                }
            }
            push @available, $vertex;
      }
        
    }
    $tick++;
    
}

print "$tick\n";
