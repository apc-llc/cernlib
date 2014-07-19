;
; $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:50:29 mclareni Exp $
;
; $Log: sbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:29  mclareni
; Kernlib
;
;
 .TITLE SBYT
;++
; CERN PROGLIB# M421    SBYT            .VERSION KERNVAX  2.05  830901
; ORIG.  A.G.MICHETTE & M.WATERS, RUTHERFORD, APRIL 79
; MOD.   J. VORBRUEGGEN 16/5/83
;
; SUBROUTINE SBYT(A, X, LX, NBITS)
; Copy the byte of size NBITS at A to the byte
; in X at postion LX; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  SBYT,^M<>

        SUBL3   #1,@12(AP),R0           ; get correct bit position
        BLSS    99$                     ; return if not valid
        INSV    @4(AP),R0,@16(AP),@8(AP); set the byte
99$:    RET
        .END
