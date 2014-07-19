;
; $Id: vdotn2.s,v 1.1.1.1 1996/02/15 17:50:32 mclareni Exp $
;
; $Log: vdotn2.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:32  mclareni
; Kernlib
;
;
 .TITLE VDOTN2
;++
; CERN PROGLIB# F121    VDOTN2          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 5/5/83
;
; FUNCTION VDOTN2(A, B, N)
; VDOTN2 = VDOT(A, B, N)**2 / (VDOT(A, A, N)*VDOT(B, B, N))
;          (i. e. square of cosine of angle between A and B)
;
; Register usage:
; R0, R1, R2    sum of A(I)*B(I), A(I)*A(I), B(I)*B(I), resp.
; R3            scratch register for multiplying
; R4, R5        addresses of A(I) and B(I), resp.
; R6            counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VDOTN2,^M<R2,R3,R4,R5,R6>

        MOVL    @12(AP),R6              ; get counter
        BLEQ    20$                     ; return if <= 0
        MOVAL   @4(AP),R4               ; get base address of A
        CLRQ    R0                      ; clear sum registers
        MOVAL   @8(AP),R5               ; get base address of B
        CLRL    R2                      ; clear sum register

10$:    MULF3   (R4),(R5),R3            ; multiply A(I) and B(I)
        ADDF2   R3,R0                   ; add result to sum
        MULF3   (R4),(R4)+,R3           ; dto. for A(I) and A(I)
        ADDF2   R3,R1
        MULF3   (R5),(R5)+,R3           ; dto. for B(I) and B(I)
        ADDF2   R3,R2
        SOBGTR  R6,10$                  ; and loop

        MULF2   R0,R0                   ; compute square of scalar product
        MULF2   R2,R1                   ; multiply dividends
        DIVF2   R1,R0                   ; divide by result
        RET

20$:    CLRF    R0                      ; return a function value of zero
        RET

        .END
