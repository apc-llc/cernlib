;
; $Id: jbytpk.s,v 1.1.1.1 1996/02/15 17:50:27 mclareni Exp $
;
; $Log: jbytpk.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:27  mclareni
; Kernlib
;
;
 .TITLE JBYTPK
;++
; CERN PROGLIB# M422    JBYTPK          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 10/5/83
;
; INTEGER FUNCTION JBYTPK(A, JA, MPACK)
; Return the JA'th byte in A
;
; Bytes are described by their length (in bits) NBITS (MPACK(1))
; and their number per longword INWORD (MPACK(2)).
; NBITS = 0 is interpreted as a default value of NBITS = 1 and INWORD = 32.
; Callers of JBYTPK assume the first byte in A to be JA = 1;
; internally, however, all counters concerning bytes start at 0.
;
; Register usage:
; R0    first used as scratch register, then contains result
; R1    first contains INWORD, then position of bit field (=byte)
; R2    = NBITS
; R3    longword offset from A to longword containing desired byte
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  JBYTPK,^M<R2,R3>

        MOVAL   @12(AP),R1              ; get base address of MPACK
        MOVL    (R1),R2                 ; get NBITS
        BEQL    10$                     ; default values required ?
        MOVL    4(R1),R1                ; no, get INWORD
        BRB     20$                     ; and skip
10$:    MOVL    #1,R2                   ; yes, NBITS = 1
        MOVL    #32,R1                  ; and INWORD = 32

20$:    MOVL    @8(AP),R0               ; get byte number
        DECL    R0                      ; change it to internal counting
        DIVL3   R1,R0,R3                ; compute longword offset
        MULL2   R3,R1                   ; number of bytes up to offset
        SUBL2   R1,R0                   ; byte number
        ASHL    #2,R3,R3                ; compute byte offset
        MULL3   R0,R2,R1                ; finally, position of first bit
        EXTZV   R1,R2,@4(AP)[R3],R0     ; extract the byte
        RET

        .END
