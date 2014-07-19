;
; $Id: vmatr.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vmatr.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VMATR
;++
; CERN PROGLIB# F121    VMATR           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 12/5/83
;
; SUBROUTINE VMATR(A, G, V, N, M)
; V(J) = SUM( G(J,I)*A(I) ); I = 1,...,N; J = 1,...,M
;
; Array dimensions: A(N), G(M,N), V(M)
; G is stored according to FORTRAN convention (i. e. column-wise).
;
; Register usage:
; R0    sum register (contains V(J) at end of inner loop)
; R1    scratch register for multiplying
; R2    = N
; R3    loop index I = 0,...,N-1 (enables use of indexing with R3)
; R4    base address of A
; R5    = M
; R6    loop index J = 1,...,M
; R7    address of V(J)
; R8    address of G(J,I)
; R9    = 4*M, i. e. displacement in bytes from G(J,I) to G(J,I+1)
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VMATR,^M<R2,R3,R4,R5,R6,R7,R8,R9>

        MOVL    @16(AP),R2              ; get N
        BLEQ    30$                     ; return if <= 0
        MOVAL   @4(AP),R4               ; get base address of A
        MOVL    @20(AP),R5              ; get M
        MOVL    #1,R6                   ; start loop of J
        MOVAL   @12(AP),R7              ; get base address of V
        ASHL    #2,R5,R9                ; compute 4*M
        MOVAL   @8(AP),R8               ; get base address of G

10$:    CLRF    R0                      ; initialize sum
        CLRL    R3                      ; start loop of I

20$:    MULF3   (R8),(R4)[R3],R1        ; compute G(J,I)*A(I)
        ADDF2   R1,R0                   ; add result to sum
        ADDL2   R9,R8                   ; compute address of G(J,I+1)
        AOBLSS  R2,R3,20$               ; and loop

        MOVF    R0,(R7)+                ; store result
        MOVAL   @8(AP)[R6],R8           ; compute address of G(J+1,1)
        AOBLEQ  R5,R6,10$               ; and loop

30$:    RET

        .END
