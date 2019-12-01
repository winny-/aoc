#lang racket
(for*/sum([n(port->list read)][m(let l([f n])((λ(g)(if(> g 0)(cons g(l g))'()))(-(quotient f 3)2)))])m)
