        section .text
        extern snake_error
        global start_here
start_here:
;;; true:  0xFFFFFFFFFFFFFFFF
;;; false: 0x7FFFFFFFFFFFFFFF
        push rbp
        push r15
        mov rbp, rsp
        ;; make sure the stack is aligned
        sub rsp, 8


	;; push the 2nd argument
        push 0
        ;;  push the 1st argument
	;; other version would get a stack overflow with this big of arg
        push 20000000
	call zero_to_n_tail

	;; restore rsp, effectively popping off the arguments we pushed
	add rsp, 16
        
        mov rsp, rbp
        pop r15
        pop rbp
        ret

	;; diamondback version:
	;; def zero_to_n_tail(n, acc):
        ;;   let cond = n == 0 in
        ;;   if cond:
        ;;     acc
        ;;   else:
        ;;     let m = n - 1 in
        ;;     let acc2 = n + acc in
        ;;     zero_to_n_tail(m, acc2)
        ;; end
	
zero_to_n_tail:
        push rbp
        mov rbp, rsp

        ;; actually only need 3 variables, but allocate 4 instead to maintain alignment
        sub rsp, 8 * 4

        ;; body of zero_to_n
        ;; QWORD says to read the memory as 4 (Q) 2-byte words (WORD), i.e., 64-bits
        cmp QWORD [rbp + 16], 0
        je eq_zero_tru
        mov rax, 0x7FFFFFFFFFFFFFFF
        jmp eq_zero_done
eq_zero_tru:
        mov rax, 0xFFFFFFFFFFFFFFFF
eq_zero_done:
	mov [rbp - 8], rax

	mov r15, 0xFFFFFFFFFFFFFFFF ; true
        cmp [rbp - 8], r15
	je thn
        ;; else

        ;; m = n - 1
        mov rax, QWORD [rbp + 16]
        sub rax, 2
        mov [rbp - 16], rax

        ;; acc2 = n + acc
        mov rax, QWORD [rbp + 24]
        add rax, QWORD [rbp + 16]
	mov [rbp - 24], rax

        ;; make a tail call rather than a normal call
	;; Step 1: overwrite our parameters with the callee's parameters
        ;; move m into first arg
        mov rax, [rbp - 16]
        mov [rbp + 16], rax

        ;; mov acc2 into second arg
        mov rax, [rbp - 24]
        mov [rbp + 24], rax

	;; Step 3: make the stack pointer point at the return address
        mov rsp, rbp
        ;; Step 4: restore rsp (and any other callee-save registers)
        pop rbp

	;; Step 5: jmp to, rather than call, the label of the function
        jmp zero_to_n_tail

thn:
        mov rax, [rbp + 24]     ; return the accumulator
        
done:   
        mov rsp, rbp
        pop rbp
        ret
        
add_err:
        mov rdi, 0
        mov rsi, rax
        call snake_error



















































        ;; zero_to_n:
;;         push rbp
;;         mov rbp, rsp
;; 	sub rsp, 8 * 4

;; 	mov rax, [rbp + 16]
;;         cmp rax, 0
;;         je eq_true
;; 	mov rax, 0x7FFFFFFFFFFFFFFF
;;         jmp eq_done
;; eq_true:
;; 	mov rax, 0xFFFFFFFFFFFFFFFF
;; eq_done:
;; 	mov [rbp - 8], rax
;;         mov rax, [rbp - 8]
;; 	cmp rax, 0xFFFFFFFFFFFFFFFF
;; 	je tru
;; 	mov rax, [rbp + 16]
;; 	sub rax, 2
;;         mov [rbp - 16], rax

;; 	;; recursive call
;;         push QWORD [rbp - 16]
;;         call zero_to_n
	
;;         mov [rbp - 24], rax
;; 	mov rax, [rbp + 16]
;; 	add rax, [rbp - 24]
;; 	jmp done
;; tru:
;; 	mov rax, 0
;; done:   
;;         mov rsp, rbp
;;         pop rbp
;;         ret

