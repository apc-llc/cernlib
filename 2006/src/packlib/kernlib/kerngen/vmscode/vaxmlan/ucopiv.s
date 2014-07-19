;
; $Id: ucopiv.s,v 1.1.1.1 1996/02/15 17:50:30 mclareni Exp $
;
; $Log: ucopiv.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:30  mclareni
; Kernlib
;
;
 .TITLE UCOPIV
;++
; CERN PROGLIB# V301    UCOPIV          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 25/5/83
;
; SUBROUTINE UCOPIV(A, X, N)
; Copy the vector A of length N into X in reverse order,
; i. e. X(1) = A(N),...,X(N) = A(1).
;
; Register usage:
; R0, R1        addresses of A(I) and X(N-I+1), resp.
; R2            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UCOPIV,^M<R2>

        MOVL    @12(AP),R2              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R0               ; get base address of A
        MOVAL   @8(AP)[R2],R1           ; get address of X(N+1)
10$:    MOVL    (R0)+,-(R1)             ; copy the value
        SOBGTR  R2,10$                  ; and loop
20$:    RET

        .END
