;
; $Id: vdotn.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vdotn.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VDOTN
;++
; CERN PROGLIB# F121    VDOTN           .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 5/5/83
;
; FUNCTION VDOTN(A, B, N)
; VDOTN = VDOT(A, B, N) / SQRT(VDOT(A, A, N)*VDOT(B, B, N))
;         (i. e. cosine of angle between A and B)
;
; A value of +/- 1 is returned if ABS(VDOTN) .GT. 1 (rounding error)
;
; Register usage:
; R0, R1        sum of A(I)*A(I) and B(I)*B(I), resp.
; R2, R3        addresses of A(I) and B(I), resp.
; R4            sum of A(I)*B(I) (has to be saved across call to MTH$SQRT_R3)
; R5            scratch register for multiplying
; R6            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VDOTN,^M<R2,R3,R4,R5,R6>

        MOVL    @12(AP),R6              ; get counter
        BLEQ    40$                     ; return if <= 0
        MOVAL   @4(AP),R2               ; get base address of A
        CLRQ    R0                      ; clear sum registers
        MOVAL   @8(AP),R3               ; get base address of B
        CLRL    R4                      ; clear sum register

10$:    MULF3   (R2),(R3),R5            ; multiply A(I) and B(I)
        ADDF2   R5,R4                   ; add result to sum
        MULF3   (R3),(R3)+,R5           ; dto. for B(I) and B(I)
        ADDF2   R5,R1
        MULF3   (R2),(R2)+,R5           ; dto. for A(I) and A(I)
        ADDF2   R5,R0
        SOBGTR  R6,10$                  ; and loop

        MULF2   R1,R0                   ; multiply dividends
        JSB     G^MTH$SQRT_R3           ; compute their square root
        DIVF3   R0,R4,R0                ; and divide scalar product by result
        BICL3   #^X8000,R0,R1           ; get absolute value of result
        CMPF    #^F1.,R1                ; do we have a rounding error ?
        BGEQ    20$
        TSTF    R0                      ; yes, test sign of result
        BLSS    30$
        MOVF    #^F1.,R0                ; positive, set VDOTN to +1.
20$:    RET
30$:    MOVF    #^F-1.,R0               ; negative, set VDOTN to -1.
        RET

40$:    CLRF    R0                      ; return a function value of zero
        RET

        .END
