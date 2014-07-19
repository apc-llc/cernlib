;
; $Id: mbytet.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: mbytet.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE MBYTET
;++
; CERN PROGLIB# M421    MBYTET          .VERSION KERNVAX  2.15  850809
; ORIG.  JZ 9/8/85, DERIVED FROM CODE BY J. VORBRUEGGEN
;
; FUNCTION MBYTET (A, X, LX, NBITS)
; Store in the byte of size NBITS in X at position LX
; the logical AND of this byte and the byte in A; the LSB is LX = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  MBYTET,^M<R2,R3>

        MOVL    @8(AP),R0               ; R0 = X
        SUBL3   #1,@12(AP),R1           ; get correct bit position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@16(AP),R0,R2        ; extract the byte at X
        EXTZV   #0,@16(AP),@4(AP),R3    ; and the one at A
        MCOML   R3,R3                   ; this is necessary ...
        BICL2   R3,R2                   ; to make BIC work as an AND
        INSV    R2,R1,@16(AP),R0        ; and save the result in X
99$:    RET

        .END
