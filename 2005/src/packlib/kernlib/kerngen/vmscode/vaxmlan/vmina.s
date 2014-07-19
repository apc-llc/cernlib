;
; $Id: vmina.s,v 1.1.1.1 1996/02/15 17:50:33 mclareni Exp $
;
; $Log: vmina.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:33  mclareni
; Kernlib
;
;
 .TITLE VMINA
;++
; CERN PROGLIB# F121    VMINA           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 4/5/83
;
; FUNCTION VMINA(A, N)
; Give minimum of absolute values of A(I), I=1,...,N
;
; Register usage:
; R0    minimum (initialized with ABS(A(1)))
; R1    absolute value of A(I)
; R2    address of A(I)
; R3    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VMINA,^M<R2,R3>

        MOVL    @8(AP),R3               ; get counter
        BLEQ    40$                     ; return if <= 0
        MOVAL   @4(AP),R2               ; get base address of A
        BICL3   #^X8000,(R2)+,R0        ; initialize minimum
        DECL    R3                      ; update counter
        BEQL    30$                     ; return if N = 1

10$:    BICL3   #^X8000,(R2)+,R1        ; get ABS(A(I))
        CMPF    R1,R0                   ; new minimum ?
        BGEQ    20$
        MOVF    R1,R0                   ; save it
20$:    SOBGTR  R3,10$                  ; loop
30$:    RET

40$:    CLRF    R0                      ; return a function value of zero
        RET

        .END
