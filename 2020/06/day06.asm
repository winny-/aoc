section .data
answers:        times 26 dq 0
sum:    dq 0
nl:     db `\n`
section .bss
char1:   resb 1
char2:   resb 2
section .text
        global _start
_start:
        call zeroit
L_answer:
        mov rax, 0
        call getc
        mov r14, rax
        cmp rax, 0
        jle B_group_end
        mov rax, [char1]
        cmp rax, `\n`
        jne B_character
        cmp r15, `\n`
        je B_group_end
        mov r15, rax
        jmp L_answer
B_character:
        mov r15, rax
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
        je B_no_incr
        inc qword [sum]
B_no_incr:      
        loop L_count
        call zeroit
        cmp r14, 0
        jg L_answer

B_end:      
	mov rax, qword [sum]
        call printi

        mov rax, 60
        mov rdi, 0
        syscall

getc:
        mov rax, 0
        mov rdi, 0
        mov rsi, char1
        mov rdx, 1
        syscall
        ret
printi:                         ; XXX this is incorrect
        mov r12, 0
        mov rdx, 0              ; Zero out upper half of dividend
        mov rbx, 10
        div rbx                 ; Q:rax R:rdx
        cmp rax, 0
        je  B_final_digit
        mov r13, rdx            ; Save remainder elsewhere
        add rax, '0'            ; int to char
        mov [char2], rax
        mov rax, 1              ; write
        mov rdi, 1              ; stdout
        mov rsi, char2          ; the buffer
        mov rdx, 1              ; 1 byte long
        syscall
        mov rax, r13
        jmp printi

B_final_digit:
        mov rax, rdx
        add rax, '0'
        mov [char2], rax
        mov rax, 1              ; write
        mov rdi, 1              ; stdout
        mov rsi, char2          ; the buffer
        mov rdx, 1              ; 1 byte long
        syscall        

        mov rax, 1
        mov rdi, 1
        mov rsi, nl
        mov rdx, 1
        syscall
        ret

zeroit:
        mov rcx, 26
L_zero:
        mov rdx, rcx
        dec rdx
        mov qword [answers+8*rdx], 0
        loop L_zero
        ret     

;; Local Variables:
;; compile-command: "nasm -g -f elf64 day06.asm && ld day06.o -o day06 && ./day06 < input.txt"
;; End:
