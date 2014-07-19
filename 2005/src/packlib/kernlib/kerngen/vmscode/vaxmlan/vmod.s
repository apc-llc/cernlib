;
; $Id: vmod.s,v 1.1.1.1 1996/02/15 17:50:33 mclareni Exp $
;
; $Log: vmod.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:33  mclareni
; Kernlib
;
;
 .TITLE VMOD
;++
; CERN PROGLIB# F121    VMOD            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 4/5/83
;
; FUNCTION VMOD(A, N)
; Give |A|, i. e. euclidian norm of N-dim. vector A
;
; Register usage:
; R0    sum of A(I)*A(I)
; R1    scratch register for multiplying
; R2    address of A(I)
; R3    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VMOD,^M<R2,R3>

        CLRF    R0                      ; initialize sum
        MOVL    @8(AP),R3               ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R2               ; get base address of A

10$:    MULF3   (R2),(R2)+,R1           ; compute square of A(I)
        ADDF2   R1,R0                   ; add result to sum
        SOBGTR  R3,10$                  ; and loop
        JSB     G^MTH$SQRT_R3           ; compute square root
20$:    RET

        .END
