section .text
        global start_here
        extern print_snake_val
        extern snake_error
        extern print_stack
start_here:
        ;;  code goes here
        push r15
        push rbp
        mov rbp, rsp

        ;; initialize our heap pointer
        mov r15, rdi

        ;; y_intercept in r8
        ;; slope in r9
        ;; pt    in r10
        ;; line  in r11
        mov r8, 6
        mov r9, 4
        mov r10, 8
        jmp done_lambda_0
lambda_0:
        push rbp
        mov rbp, rsp
        ;;
        sub rsp, 16

        ;; load self into rax
        mov rax, [rbp + 16]

        ;; load slope
        mov rcx, [rax + 3 * 8]
        mov [rbp - 8], rcx

        ;; load y_intercept
        mov rcx, [rax + 4 * 8]
        mov [rbp - 8 * 2], rcx

        mov rax, [rbp + 8 * 3]
	sar rax, 1
        imul rax, [rbp - 8]
        jo overflow_err
        add rax, [rbp - 8 * 2]

        mov rsp, rbp
        pop rbp
        ret
        ;; x: slope * x + y_intercept
done_lambda_0:
        ;; construct the closure
        mov rax, lambda_0
        mov [r15], rax
        mov QWORD [r15 + 8], 1
        mov QWORD [r15 + 8 * 2], 2
        mov [r15 + 8 * 3], r9
        mov [r15 + 8 * 4], r8

        mov r11, r15
        add r11, 0x5

        ;; increment the heap pointer
        add r15, 8 * 5

	;; check that line is a closure
        mov rax, r11
        and rax, 0x7
        cmp rax, 0x5
	jne not_a_function_err

        sub r11, 0x5

        ;; check the arity
        mov rax, [r11 + 8]
        cmp rax, 1
	jne arity_err
        
        ;; make the call
        ;; push arg
        push r10
        ;; push self
        push r11
        call [r11]
        

;; uncomment, IF YOU DARE
;;         mov r10, 50
;; danger_loop:
;;         mov QWORD [r15], 3
;;         add r15, 8
;;         cmp r10, 0
;;         sub r10, 1
;;         jg danger_loop

        

        ;; mov rax, r15

        mov rsp, rbp
        pop rbp
        pop r15
        ret
        
        
        ;; calling line(pt)
        
overflow_err:
        mov rdi, 0
        call snake_error
not_a_function_err:     
arity_err:
arith_err:
        mov rdi, 1
        call snake_error
cmp_err:
        mov rdi, 2
        call snake_error
log_err:
        mov rdi, 3
        call snake_error
if_err:
        mov rdi, 4
        call snake_error
        
        
