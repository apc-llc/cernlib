;
; $Id: vexcum.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vexcum.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VEXCUM
;++
; CERN PROGLIB# F121    VEXCUM          .VERSION KERNVAX  2.05  830901
; ORIG. J. VORBRUEGGEN 4/5/83
;
; SUBROUTINE VEXCUM(A, EX, N)
; EX(1) = MIN(EX(1), A(I)) , I=1,...,N
; EX(2) = MAX(EX(2), A(I)) , I=1,...,N
; EX(3) = EX(3) + A(I)     , I=1,...,N
;
; Register usage:
; R0    minimum, initialized with EX(1)
; R1    maximum, initialized with EX(2)
; R2    cumulative sum, initialized with EX(3)
; R3    address of A(I)
; R4    base address of EX
; R5    counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VEXCUM,^M<R2,R3,R4,R5>

        MOVL    @12(AP),R5              ; get counter
        BLEQ    40$                     ; return if <= 0
        MOVAL   @8(AP),R4               ; get base addresses of EX and A
        MOVAL   @4(AP),R3
        MOVF    (R4),R0                 ; initialize minimum
        MOVF    4(R4),R1                ;            maximum
        MOVF    8(R4),R2                ;            sum

10$:    CMPF    (R3),R0                 ; new minimum ?
        BGEQ    20$
        MOVF    (R3),R0                 ; yes, save it
20$:    CMPF    R1,(R3)                 ; new maximum ?
        BGEQ    30$
        MOVF    (R3),R1                 ; yes, save it
30$:    ADDF2   (R3)+,R2                ; update the sum
        SOBGTR  R5,10$                  ; and loop

        MOVQ    R0,(R4)                 ; write back EX(1), EX(2) ...
        MOVL    R2,8(R4)                ; and EX(3)
40$:    RET

        .END
