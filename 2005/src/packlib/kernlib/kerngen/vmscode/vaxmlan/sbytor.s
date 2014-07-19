;
; $Id: sbytor.s,v 1.1.1.1 1996/02/15 17:50:29 mclareni Exp $
;
; $Log: sbytor.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:29  mclareni
; Kernlib
;
;
 .TITLE SBYTOR
;++
; CERN PROGLIB# M421    SBYTOR          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 16/5/83
;
; SUBROUTINE SBYTOR(A, X, LX, NBITS)
; Store in the byte of size NBITS in X at position LX
; the logical OR of this byte and the byte in A; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  SBYTOR,^M<R2>

        SUBL3   #1,@12(AP),R2           ; get correct bit position
        BLSS    99$                     ; return if not valid
        EXTZV   R2,@16(AP),@8(AP),R0    ; extract the byte at X
        EXTZV   #0,@16(AP),@4(AP),R1    ; and the one at A
        BISL2   R1,R0                   ; OR them
        INSV    R0,R2,@16(AP),@8(AP)    ; and save the result in X
99$:    RET

        .END
