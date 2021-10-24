# A fun DAG problem... Let's learn some Perl5!

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
    push @available, $vertex;
}

while (@available) {
    @available = sort @available;
    # print "Available: ", join('', @available), " Finished: ", join('', @finished), "\n";
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

# keys %graph;
# while (my ($vertex, $edges) = each %graph) {
#     foreach (@{$edges}) {
#         print "$vertex $_\n";
#     }
# }

$result = join '', @finished;
print "$result\n";
