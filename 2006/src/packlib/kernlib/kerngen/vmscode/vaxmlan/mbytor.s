;
; $Id: mbytor.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: mbytor.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE MBYTOR
;++
; CERN PROGLIB# M421    MBYTOR          .VERSION KERNVAX  2.15  850809
; ORIG.  JZ 9/8/85, DERIVED FROM CODE BY J. VORBRUEGGEN
;
; FUNCTION MBYTOR (A, X, LX, NBITS)
; Store in the byte of size NBITS in X at position LX
; the logical OR of this byte and the byte in A; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  MBYTOR,^M<R2,R3>

        MOVL    @8(AP),R0               ; R0 = X
        SUBL3   #1,@12(AP),R1           ; get correct bit position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@16(AP),R0,R2        ; extract the byte at X
        EXTZV   #0,@16(AP),@4(AP),R3    ; and the one at A
        BISL2   R3,R2                   ; OR them
        INSV    R2,R1,@16(AP),R0        ; and save the result in X
99$:    RET

        .END
