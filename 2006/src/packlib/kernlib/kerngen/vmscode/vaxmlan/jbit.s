;
; $Id: jbit.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: jbit.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE JBIT
;++
; CERN PROGLIB# M421    JBIT            .VERSION KERNVAX  2.05  830901
; ORIG.  A.G.MICHETTE & M.WATERS, RUTHERFORD, APRIL 79
; MOD.   J. VORBRUEGGEN 16/5/83
;
; INTEGER FUNCTION JBIT(A, LA)
; Return the bit LA of A, the LSB is LA = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JBIT,^M<>

        SUBL3   #1,@8(AP),R1            ; get correct bit position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,#1,@4(AP),R0         ; get the byte
99$:    RET
        .END
