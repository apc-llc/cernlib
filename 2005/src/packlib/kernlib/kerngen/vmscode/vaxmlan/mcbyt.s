;
; $Id: mcbyt.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: mcbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE MCBYT
;++
; CERN PROGLIB# M421    MCBYT           .VERSION KERNVAX  2.15  850809
; ORIG.  JZ 9/8/85, DERIVED FROM CODE BY J. VORBRUEGGEN
;
; FUNCTION MCBYT (A, LA, X, LX, NBITS)
; Copy the byte of size NBITS in A at position LA
; to the byte in X at postion LX; the LSB is LA = LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  MCBYT,^M<R2>

        MOVL    @12(AP),R0              ; R0 = X
        SUBL3   #1,@8(AP),R1            ; get correct byte position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@20(AP),@4(AP),R2    ; extract the byte
        SUBL3   #1,@16(AP),R1           ; get new position
        BLSS    99$                     ; return if not valid
        INSV    R2,R1,@20(AP),R0        ; copy the byte
99$:    RET
        .END
