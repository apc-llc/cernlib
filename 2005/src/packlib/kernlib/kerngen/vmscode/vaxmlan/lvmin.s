;
; $Id: lvmin.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: lvmin.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE LVMIN
;++
; CERN PROGLIB# F121    LVMIN           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 11/5/83
;
; INTEGER FUNCTION LVMIN(A, N)
; Give location (index) of minimum of A(I), I=1,...,N
;
; Register usage:
; R0    index of minimum
; R1    minimum (initialized with A(1))
; R2    address of A(I)
; R3    loop index
; R4    = N
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  LVMIN,^M<R2,R3,R4>

        MOVL    @8(AP),R4               ; get N
        BLEQ    40$                     ; return if <= 0
        MOVL    #1,R0                   ; initialize index
        DECL    R4                      ; update N
        BEQL    30$                     ; return if N = 1
        MOVAL   @4(AP),R2               ; get base address of A
        MOVF    (R2)+,R1                ; initialize minimum
        MOVL    R0,R3                   ; set up loop index

10$:    CMPF    (R2)+,R1                ; new minimum ?
        BGEQ    20$
        MOVF    -4(R2),R1               ; yes, save it
        ADDL3   #1,R3,R0                ; and set up correct index
20$:    AOBLEQ  R4,R3,10$               ; loop
30$:    RET

40$:    CLRL    R0                      ; return a function value of zero
        RET

        .END
