\ Note: I'm pretty sure I have the stack effect notations reversed...  And the
\ notation itself isn't consistent.  That's a TODO item =)

create tape 2000 cells allot

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

: forward ( d d d d -- d d d )
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

: dump-state ( d d d -- )
    '[' emit . . . ']' emit ;

: dump-instruction ( c d -- )
    emit . ;

: step1 ( c u d d d -- d d d d )
    \ 4 pick 4 pick 4 pick dump-state
    \ space 2dup dump-instruction
    case
        'N' of rot swap - swap endof
        'S' of rot + swap endof
        'E' of + endof
        'W' of - endof
        'L of negate turn endof
        'R' of turn endof
        'F' of forward endof
    endcase
    \ 2 pick 2 pick 2 pick dump-state cr
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

\ TODO
: part2
    drop 42 . cr ;
    

read-instructions
\ tape . cr
\ tape 1 pick 2 * cells dump
dup dup
part1
part2
bye
