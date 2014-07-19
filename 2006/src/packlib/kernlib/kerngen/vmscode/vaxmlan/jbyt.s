;
; $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: jbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE JBYT
;++
; CERN PROGLIB# M421    JBYT            .VERSION KERNVAX  2.05  830901
; ORIG.  A.G.MICHETTE & M.WATERS, RUTHERFORD, APRIL 79
; MOD.   J. VORBRUEGGEN 16/5/83
;
; INTEGER FUNCTION JBYT(A, LA, NBITS)
; Return the byte of size NBITS in A at position LA; the LSB is LA = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JBYT,^M<>

        SUBL3   #1,@8(AP),R1            ; get correct byte position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@12(AP),@4(AP),R0    ; extract the byte
99$:    RET
        .END
