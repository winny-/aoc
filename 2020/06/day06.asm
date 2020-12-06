section .data
answers:        times 26 dq 0
sum1:   dq 0
sum2:   dq 0
nl:     db `\n`
section .bss
char1:   resb 1
char2:   resb 2
section .text
        global _start
_start:
B_read_record:  
        xor r12, r12            ; Zero # of persons read for this record.
        mov rcx, 26
L_zero:
        mov rdx, rcx
        dec rdx
        mov qword [answers+8*rdx], 0 ; Some reason [answers+8*rcx-1] does not work...
        loop L_zero
L_answer:
        call getc
        mov r14, rax            ; Save retval for later use.
        cmp rax, 0
        jle B_group_end         ; Finished input or error.
        mov rax, [char1]        ; Get the character read.
        cmp rax, `\n`
        jne B_character         ; NOT a NL -> character counting logic.
        cmp r15, `\n`
        je B_group_end          ; Double NL -> finished with record.
        inc r12                 ; New person, but not new record, so incr count.
        mov r15, rax            ; Save the character for checking double NL.
        jmp L_answer
B_character:
        mov r15, rax            ; Save the character for checking double NL.
        sub rax, 'a'
        inc qword [answers+8*rax]
        jmp L_answer
B_group_end:
        mov rcx, 26
L_count:
        mov rdx, rcx
        dec rdx
        mov rax, qword [answers+8*rdx]
        cmp rax, 0
        je B_no_incr            ; If no occurrence -> skip counting this letter.
        inc qword [sum1]
        cmp rax, r12
        jne B_no_incr           ; If person count != occurrence count -> skip part2 incr.
        inc qword [sum2]
B_no_incr:
        loop L_count
        cmp r14, 0
        jg B_read_record        ; If the retval from the read was ok, continue reading.

B_end:      
	mov rax, qword [sum1]   ; Part 1
        call printi
	
        mov rax, qword [sum2]   ; Part 2
        call printi

        mov rax, 60             ; Exit
        mov rdi, 0
        syscall
        ;; Is not reached.

getc:
        ;; Read input from stdin on character at a time, store result in
        ;; `char1'.
        mov rax, 0
        mov rdi, 0
        mov rsi, char1
        mov rdx, 1
        syscall
        ret
printi:
        ;; Print number in RAX as decimal.  Probably still has bugs in
	;; it... but it works.
        xor r11, r11            ; Zero out count of digits.
L_n2s:  
        xor rdx, rdx            ; Zero out upper half of dividend
        mov rbx, 10             ; Divisor
        div rbx                 ; RDX:RAX / RBX -> Q:RAX R:RDX
        cmp rax, 0
        je B_final_digit
        inc r11
        push rdx
        jmp L_n2s
B_final_digit:
        cmp rdx, 0
        je B_is_zero
        inc r11
        push rdx        
        jmp B_do_print
B_is_zero:
        cmp r11, 0
        jne B_do_print
        inc r11
        push qword 0
B_do_print:
        mov rcx, r11
L_do_print:     
        pop rbx
        add rbx, '0'
        push rcx                ; Save rcx which can be messed up by syscalls.
        mov [char2], rbx
        mov rax, 1
        mov rdi, 1
        mov rsi, char2
        mov rdx, 1
        syscall
        pop rcx
        loop L_do_print
        ;; Print NL
        mov rax, 1,
        mov rdi, 1
        mov rsi, nl
        mov rdx, 1
        syscall
        ret

;; Local Variables:
;; compile-command: "nasm -g -f elf64 day06.asm && ld day06.o -o day06 && ./day06 < input.txt"
;; End:
