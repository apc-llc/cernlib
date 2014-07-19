;
; $Id: jbytor.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: jbytor.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE JBYTOR
;++
; CERN PROGLIB# M421    JBYTOR          .VERSION KERNVAX  2.22  870524
; ORIG.  JZ 20/5/87, DERIVED FROM CODE BY J. VORBRUEGGEN
;
; INTEGER FUNCTION JBYTOR(A, B, LB, NBITS)
; Return the logical OR of A and the byte of size NBITS
; in B at position LB; the LSB is LB = 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JBYTOR,^M<>

        SUBL3   #1,@12(AP),R1           ; get correct byte position
        BLSS    99$                     ; return if not valid
        EXTZV   R1,@16(AP),@8(AP),R0    ; extract byte in B into R0
        MOVL    @4(AP),R1               ; R1 = A
        BISL2   R1,R0                   ; OR R1 into R0
99$:    RET

        .END
