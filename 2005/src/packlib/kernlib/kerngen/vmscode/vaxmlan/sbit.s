;
; $Id: sbit.s,v 1.1.1.1 1996/02/15 17:50:29 mclareni Exp $
;
; $Log: sbit.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:29  mclareni
; Kernlib
;
;
 .TITLE SBIT
;++
; CERN PROGLIB# M421    SBIT            .VERSION KERNVAX  2.05  830901
; ORIG.  A.G.MICHETTE & M.WATERS, RUTHERFORD, APRIL 79
; MOD.   J. VORBRUEGGEN 16/5/83
;
; SUBROUTINE SBIT(A, X, LX)
; Copy bit 1 of A into bit LX of X; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  SBIT,^M<>

        SUBL3   #1,@12(AP),R0           ; get correct bit position
        BLSS    99$                     ; return if not valid
        INSV    @4(AP),R0,#1,@8(AP)     ; copy the bit
99$:    RET
;++
; SUBROUTINE SBIT0(X, LX)
; Clear bit LX of X
;--
        .ENTRY  SBIT0,^M<>

        SUBL3   #1,@8(AP),R0            ; get correct bit position
        BLSS    99$                     ; return if not valid
        BBCC    R0,@4(AP),99$           ; assuming bit set will avoid branch
99$:    RET
;++
; SUBROUTINE SBIT1(X, LX)
; Set bit LX of X
;--
        .ENTRY  SBIT1,^M<>

        SUBL3   #1,@8(AP),R0            ; get correct bit position
        BLSS    99$                     ; return if not valid
        BBSS    R0,@4(AP),99$           ; assuming bit clear will avoid branch
99$:    RET
        .END
