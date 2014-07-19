;
; $Id: vmatl.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vmatl.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VMATL
;++
; CERN PROGLIB# F121    VMATL           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 12/5/83
;
; SUBROUTINE VMATL(G, C, X, N, M)
; X(J) = SUM( C(I)*G(I,J) ); I = 1,...,M; J = 1,...,N
;
; Array dimensions: C(M), G(M,N), X(N)
; G is stored according to FORTRAN convention (i. e. column-wise).
;
; Register usage:
; R0    sum register (contains X(J) at end of inner loop)
; R1    scratch register for multiplying
; R2    = M
; R3    loop index I = 0,...,M-1 (enables use of indexing with R3)
; R4    base address of C
; R5    address of X(J)
; R6    address of G(I,J)
; R7    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VMATL,^M<IV,R2,R3,R4,R5,R6,R7>

        MOVL    @20(AP),R2              ; get M
        BLEQ    30$                     ; return if <= 0
        MOVAL   @8(AP),R4               ; get base address of C
        MOVAL   @12(AP),R5              ; get base address of X
        MOVAL   @4(AP),R6               ; get base address of G
        MOVL    @16(AP),R7              ; get N

10$:    CLRF    R0                      ; initialize sum
        CLRL    R3                      ; start loop of I

20$:    MULF3   (R4)[R3],(R6)+,R1       ; compute C(I)*G(I,J)
        ADDF2   R1,R0                   ; add result to sum
        AOBLSS  R2,R3,20$               ; and loop

        MOVF    R0,(R5)+                ; store result
        SOBGTR  R7,10$                  ; and loop

30$:    RET

        .END
