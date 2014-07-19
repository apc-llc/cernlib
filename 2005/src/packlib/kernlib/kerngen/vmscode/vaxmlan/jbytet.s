;
; $Id: jbytet.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: jbytet.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE JBYTET
;++
; CERN PROGLIB# M421    JBYTET          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 16/5/83
;
; INTEGER FUNCTION JBYTET(A, B, LB, NBITS)
; Return the logical AND of the byte of size NBITS at A
; and the byte in B at position LB; the LSB is LB = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JBYTET,^M<>

        SUBL3   #1,@12(AP),R1           ; get correct byte position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@16(AP),@8(AP),R0    ; extract byte in B
        EXTZV   #0,@16(AP),@4(AP),R1    ; and the one in A
        MCOML   R1,R1                   ; this is necessary ...
        BICL2   R1,R0                   ; to make BIC work as an AND
99$:    RET

        .END
