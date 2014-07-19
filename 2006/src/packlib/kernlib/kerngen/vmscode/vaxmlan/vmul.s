;
; $Id: vmul.s,v 1.1.1.1 1996/02/15 17:50:33 mclareni Exp $
;
; $Log: vmul.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:33  mclareni
; Kernlib
;
;
 .TITLE VMUL
;++
; CERN PROGLIB# F121    VMUL            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 30/4/83
;
; SUBROUTINE VMUL(A, B, X, N)
; X(I) = A(I) * B(I) , I=1,...,N
;
; Register usage:
; R0, R1, R2    addresses of A(I), B(I), and X(I), resp.
; R3            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VMUL,^M<R2,R3>

        MOVL    @16(AP),R3              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base addresses of A, B, and X
        MOVAL   @8(AP),R1
        MOVAL   @12(AP),R2

10$:    MULF3   (R0)+,(R1)+,(R2)+       ; multiply
        SOBGTR  R3,10$                  ; and loop
20$:    RET

        .END
