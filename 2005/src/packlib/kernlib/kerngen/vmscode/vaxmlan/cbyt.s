;
; $Id: cbyt.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: cbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE CBYT
;++
; CERN PROGLIB# M421    CBYT            .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 16/5/83
;
; SUBROUTINE CBYT(A, LA, X, LX, NBITS)
; Copy the byte of size NBITS in A at position LA
; to the byte in X at postion LX; the LSB is LA = LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  CBYT,^M<>

        SUBL3   #1,@8(AP),R1            ; get correct byte position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@20(AP),@4(AP),R0    ; extract the byte
        SUBL3   #1,@16(AP),R1           ; get new position
        BLSS    99$                     ; return if not valid
        INSV    R0,R1,@20(AP),@12(AP)   ; copy the byte
99$:    RET
        .END
