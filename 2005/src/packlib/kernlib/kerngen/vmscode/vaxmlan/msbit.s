;
; $Id: msbit.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: msbit.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE MSBIT
;++
; CERN PROGLIB# M421    MSBIT           .VERSION KERNVAX  2.15  850809
; ORIG.  JZ 9/8/85, DERIVED FROM CODE BY J. VORBRUEGGEN
;
; FUNCTION MSBIT (A, X, LX)
; Copy bit 1 of A into bit LX of X; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  MSBIT,^M<>

        MOVL    @8(AP),R0               ; R0 = X
        SUBL3   #1,@12(AP),R1           ; get correct bit position
        BLSS    99$                     ; return if not valid
        INSV    @4(AP),R1,#1,R0         ; copy the bit
99$:    RET
;++
; FUNCTION MSBIT0 (X, LX)
; Clear bit LX of X
;--
        .ENTRY  MSBIT0,^M<>

        MOVL    @4(AP),R0               ; R0 = X
        SUBL3   #1,@8(AP),R1            ; get correct bit position
        BLSS    99$                     ; return if not valid
        BBCC    R1,R0,99$               ; assuming bit set will avoid branch
99$:    RET
;++
; FUNCTION MSBIT1 (X, LX)
; Set bit LX of X
;--
        .ENTRY  MSBIT1,^M<>

        MOVL    @4(AP),R0               ; R0 = X
        SUBL3   #1,@8(AP),R1            ; get correct bit position
        BLSS    99$                     ; return if not valid
        BBSS    R1,R0,99$               ; assuming bit clear will avoid branch
99$:    RET
        .END
