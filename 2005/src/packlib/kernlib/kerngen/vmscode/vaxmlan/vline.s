;
; $Id: vline.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vline.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VLINE
;++
; CERN PROGLIB# F121    VLINE           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 2/5/83
;
; SUBROUTINE VLINE(A, ALPHA, B, BETA, X, N)
; X(I) = A(I) * ALPHA  +  B(I) * BETA , I=1,...,N
;
; Register usage:
; R0, R2, R4    addresses of A(I), B(I), and X(I), resp.
; R1, R3        = ALPHA and BETA, resp.
; R5, R6        scratch registers for multiplying
; R7            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VLINE,^M<R2,R3,R4,R5,R6,R7>

        MOVL    @24(AP),R7              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get argument addresses and values
        MOVL    @8(AP),R1
        MOVAL   @12(AP),R2
        MOVL    @16(AP),R3
        MOVAL   @20(AP),R4

10$:    MULF3   (R0)+,R1,R5             ; A(I)*ALPHA
        MULF3   (R2)+,R3,R6             ; B(I)*BETA
        ADDF3   R5,R6,(R4)+             ; return sum
        SOBGTR  R7,10$                  ; and loop
20$:    RET

        .END
