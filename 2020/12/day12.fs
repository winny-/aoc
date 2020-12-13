\ Note: I'm pretty sure I have the stack effect notations reversed...  And the
\ notation itself isn't consistent.  That's a TODO item =)

create tape 2000 cells allot

: 4dup ( w1 w2 w3 w4 -- w1 w2 w3 w4 w1 w2 w3 w4 )
    3 pick 3 pick 3 pick 3 pick ;


: 3dup ( w1 w2 w3 -- w1 w2 w3 w1 w2 w3 )
    2 pick 2 pick 2 pick ;

: read-instruction ( f -- err c n )
    pad 80 stdin read-line
    invert swap invert and if drop 0 0 -1 exit endif
    pad 1+ swap 1- s>number? invert if 0 0 -1 exit endif drop
    pad c@
    0
;

: unit ( d -- d d )
    360 mod
    case
        0 of -1 0 endof
        180 of 1 0 endof
        90 of 0 1 endof
        270 of 0 -1 endof
    endcase ;

: forward1 ( d d d d -- d d d )
    3 roll dup
    unit
    3 roll tuck * -rot * 4 roll + swap 3 roll +
;

: turn ( d d d d -- d d d )
    2swap swap rot
    + 360 mod
    swap rot ;

: distance ( d d -- u )
    abs swap abs + ;

: dump-state1 ( d d d -- )
    '[' emit . . . ']' emit ;

: dump-instruction ( c d -- )
    emit . ;

: step1 ( c u d d d -- d d d d )
    \ 4 pick 4 pick 4 pick dump-state1
    \ space 2dup dump-instruction
    case
        'N' of rot swap - swap endof
        'S' of rot + swap endof
        'E' of + endof
        'W' of - endof
        'L of negate turn endof
        'R' of turn endof
        'F' of forward1 endof
    endcase
    \ 2 pick 2 pick 2 pick dump-state1 cr
;

: read-instructions
    0
    begin
        read-instruction
        if 2drop exit endif
        2 pick 2 * cells tape + dup
        cell+ 2 roll swap !
        !
        1+
    again ;

: get-instruction ( n -- c n )
    2 * cells tape + dup
    @
    swap cell+ @ ;

: dump-instructions
    0
    ?do
        i get-instruction
        dump-instruction cr
    loop ;

: part1
    \ State is x,y,dir with x at top of stack.
    90 0 0
    3 roll 0 ?do
        i get-instruction step1
    loop 
    distance . cr drop ;

: move-waypoint
    4 roll + swap  4 roll + swap 2swap ;

: rotate-waypoint
    over 0 = if 2drop exit endif
    swap 90 - swap dup
    6 roll 6 roll 3 roll
    case
        'L' of swap negate endof
        'R' of negate swap endof
    endcase
    2rot 2rot
    recurse
;

: forward2
    dup 5 pick * swap 4 pick * rot + rot rot + swap ;

: dump-state2
    2swap '[' emit . . . . ']' emit space ;

: step2
    \ 2rot 2rot 4dup dump-state2 2rot
    \ 2dup dump-instruction
    case
        'N' of negate 0 swap move-waypoint endof
        'S  of 0 swap move-waypoint endof
        'E' of 0 move-waypoint endof
        'W' of negate 0 move-waypoint endof
        'L' of 'L' rotate-waypoint endof
        'R' of 'R' rotate-waypoint endof
        'F' of forward2 endof
    endcase
    \ 4dup dump-state2 cr
    ;
: part2
    10 -1 0 0
    4 roll 0 ?do
        i get-instruction step2
    loop
    \ cr
    distance . cr drop ;

read-instructions
\ tape . cr
\ tape 1 pick 2 * cells dump
dup dup
part1
\ cr
part2
bye

\ Local Variables:
\ compile-command: "gforth day12.fs < input.txt"
\ End:
