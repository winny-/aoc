#lang racket
(for/sum([n(port->list read)])(-(quotient n 3)2))
