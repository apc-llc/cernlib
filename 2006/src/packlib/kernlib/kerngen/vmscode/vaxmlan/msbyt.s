;
; $Id: msbyt.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: msbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE MSBYT
;++
; CERN PROGLIB# M421    MSBYT           .VERSION KERNVAX  2.15  850809
; ORIG.  JZ 9/8/85, DERIVED FROM CODE BY J. VORBRUEGGEN
;
; FUNCTION MSBYT (A, X, LX, NBITS)
; Copy the byte of size NBITS at A to the byte
; in X at postion LX; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  MSBYT,^M<>

        MOVL    @8(AP),R0               ; R0 = X
        SUBL3   #1,@12(AP),R1           ; get correct bit position
        BLSS    99$                     ; return if not valid
        INSV    @4(AP),R1,@16(AP),R0    ; set the byte
99$:    RET
        .END
