;
; $Id: lvmaxa.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: lvmaxa.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE LVMAXA
;++
; CERN PROGLIB# F121    LVMAXA          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 11/5/83
;
; INTEGER FUNCTION LVMAXA(A, N)
; Give location (index) of maximum of absolute values of A(I), I=1,...,N
;
; Register usage:
; R0    index of maximum
; R1    maximum (initialized with ABS(A(1)))
; R2    absolute value of A(I)
; R3    address of A(I)
; R4    loop index
; R5    = N
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  LVMAXA,^M<R2,R3,R4,R5>

        MOVL    @8(AP),R5               ; get N
        BLEQ    40$                     ; return if <= 0
        MOVL    #1,R0                   ; initialize index
        DECL    R5                      ; update N
        BEQL    30$                     ; return if N = 1
        MOVAL   @4(AP),R3               ; get base address of A
        BICL3   #^X8000,(R3)+,R1        ; initialize maximum
        MOVL    R0,R4                   ; set up loop index

10$:    BICL3   #^X8000,(R3)+,R2        ; get absolute value of A(I)
        CMPF    R1,R2                   ; new maximum ?
        BGEQ    20$
        MOVF    R2,R1                   ; yes, save it
        ADDL3   #1,R4,R0                ; and set up correct index
20$:    AOBLEQ  R5,R4,10$               ; loop
30$:    RET

40$:    CLRL    R0                      ; return a function value of zero
        RET

        .END
