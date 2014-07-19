;
; $Id: vdot.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vdot.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VDOT
;++
; CERN PROGLIB# F121    VDOT            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 4/5/83
;
; FUNCTION VDOT(A, B, N)
; Scalar product of N-dim. vectors A and B
;
; Register usage:
; R0            product of A and B
; R1            scratch register for multiplying
; R2, R3        addresses of A(I) and B(I), resp.
; R4            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VDOT,^M<R2,R3,R4>

        CLRF    R0                      ; initialize product
        MOVL    @12(AP),R4              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R2               ; get base addresses of A and B
        MOVAL   @8(AP),R3

10$:    MULF3   (R2)+,(R3)+,R1          ; multiply A(I) and B(I)
        ADDF2   R1,R0                   ; add result to product
        SOBGTR  R4,10$                  ; and loop
20$:    RET

        .END
