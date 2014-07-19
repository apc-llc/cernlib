;
; $Id: vunit.s,v 1.1.1.1 1996/02/15 17:50:34 mclareni Exp $
;
; $Log: vunit.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:34  mclareni
; Kernlib
;
;
 .TITLE VUNIT
;++
; CERN PROGLIB# F121    VUNIT           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 4/5/83
;
; SUBROUTINE VUNIT(A, X, N)
; X(I) = A(I) / |A| , I=1,...,N
;
; Register usage:
;         Part I                          Part II
; R0    sum of A(I)**2                  value of |A|
; R1    address of A(I)                 address of A(I)
; R2    scratch for multiplying         address of X(I)
; R3    counter from N to 1             counter
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VUNIT,^M<R2,R3>
                                        ; Part I:
        MOVL    @12(AP),R3              ; get counter
        BLEQ    30$                     ; return if <= 0
        MOVAL   @4(AP),R1               ; get base address of A
        CLRF    R0                      ; initialize sum

10$:    MULF3   (R1),(R1)+,R2           ; compute square of A(I)
        ADDF2   R2,R0                   ; add result to sum
        SOBGTR  R3,10$                  ; and loop
                                        ; Part II:
        JSB     G^MTH$SQRT_R3           ; compute |A|
        MOVAL   @4(AP),R1               ; get base address of A
        DIVF3   R0,#^F1.,R0             ; compute 1./|A|
        MOVAL   @8(AP),R2               ; get base address of X
        MOVL    @12(AP),R3              ; set up counter again

20$:    MULF3   R0,(R1)+,(R2)+          ; scale A(I)
        SOBGTR  R3,20$                  ; and loop
30$:    RET

        .END
