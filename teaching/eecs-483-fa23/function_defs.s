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

        ;; push a junk argument so that the stack is aligned again 

	;; (we'll discuss on Monday why we align here and at the beginning of the function)
        push 0xabcd
        ;;  push the argument
        ;;  10 is the representation of 5
        push 10

        ;; the following instead causes a stack overflow:
        ;; push 1000000     
	call zero_to_n

	;; restore rsp, effectively popping off the arguments we pushed
	add rsp, 16
        
        mov rsp, rbp
        pop r15
        pop rbp
        ret

zero_to_n:
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

	mov r15, 0xFFFFFFFFFFFFFFFF
        cmp [rbp - 8], r15
	je thn
        ;; else

        mov rax, QWORD [rbp + 16]
        sub rax, 2
        mov [rbp - 16], rax

	;; push a junk value again
        push 0xffde
        push QWORD [rbp - 16]
        call zero_to_n
	add rsp, 16
        
        mov [rbp - 24], rax

        mov rax, [rbp + 16]
        mov r15, [rbp - 24]
        add rax, r15
        jmp done

thn:
        mov rax, 0
        
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

