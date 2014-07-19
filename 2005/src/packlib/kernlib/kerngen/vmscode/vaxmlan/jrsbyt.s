;
; $Id: jrsbyt.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: jrsbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE JRSBYT
;++
; CERN PROGLIB# M421    JRSBYT          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 16/5/83
;
; INTEGER FUNCTION JRSBYT(A, X, LX, NBITS)
; Return the byte of size NBITS in X at position LX
; and set it to the byte at A; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JRSBYT,^M<>

        SUBL3   #1,@12(AP),R1           ; get correct byte position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@16(AP),@8(AP),R0    ; extract ol byte
        INSV    @4(AP),R1,@16(AP),@8(AP); and insert new one
99$:    RET

        .END
