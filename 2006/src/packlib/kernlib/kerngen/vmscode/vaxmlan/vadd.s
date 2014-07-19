;
; $Id: vadd.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vadd.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VADD
;++
; CERN PROGLIB# F121    VADD            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 30/4/83
;
; SUBROUTINE VADD(A, B, X, N)
; X(I) = A(I) + B(I) , I=1,...,N
;
; Register usage:
; R0, R1, R2    addresses of A(I), B(I), and X(I), resp.
; R3            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VADD,^M<R2,R3>

        MOVL    @16(AP),R3              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base addresses of A, B, and X
        MOVAL   @8(AP),R1
        MOVAL   @12(AP),R2
10$:    ADDF3   (R0)+,(R1)+,(R2)+       ; add
        SOBGTR  R3,10$                  ; and loop
20$:    RET

        .END
