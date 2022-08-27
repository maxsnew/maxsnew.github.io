        section .text
        global start_here
        extern snake_error
start_here:
;;; true:  0xFFFFFFFFFFFFFFFF
;;; false: 0x7FFFFFFFFFFFFFFF

        push rbp
        push r15
        mov rbp, rsp
        sub rsp, 3 * 8

        ;; let x = true in let y = 7 in x + y

        ;; true
        mov rax, 0xFFFFFFFFFFFFFFFF

        ;; let x =
        mov [rbp - 8], rax

        ;; 7
        mov rax, 14

	;; let y =
        mov [rbp - 16], rax

        ;; x + y
        mov rax, [rbp - 8]
	mov rsi, rax
        test rsi, 0x01
        jne plus_fail

	mov r15, [rbp - 16]
        test r15, 0x01
        jne plus_fail

        add rax, r15

        mov rsp, rbp
        pop r15
        pop rbp
        ret

;;; expects offending value in rsi
plus_fail:
        mov rdi, 1
        call snake_error

add1_fail:
        mov rdi, 0
        call snake_error
