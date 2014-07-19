;
; $Id: upkbyt.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: upkbyt.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UPKBYT
;++
; CERN PROGLIB# M422    UPKBYT          .VERSION KERNVAX  2.05  830901
; ORIG.  J. VORBRUEGGEN 10/5/83
;
; SUBROUTINE UPKBYT(A, JA, IY, N, MPACK)
; Unpack the N bytes from A, staring at the JA'th byte,
; into the array of small integers starting at IY.
;
; Bytes are described by their length (in bits) NBITS (MPACK(1))
; and their number per longword INWORD (MPACK(2)).
; NBITS = 0 is interpreted as a default value of NBITS = 1 and INWORD = 32.
; Callers of UPKBYT assume the first byte in A to be JA = 1;
; internally, however, all counters concerning bytes start at 0.
;
; Register usage:
; R0    = NBITS
; R1    = INWORD
; R2    base address of bit field (=byte)
; R3    byte number in current longword (from 0 to INWORD-1)
; R4    position of bit field (from 0 to (INWORD-1)*NBITS)
; R5    updated address in IY
; R6    first used as scratch register, then counter from N to 1
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UPKBYT,^M<R2,R3,R4,R5,R6>

        MOVAL   @20(AP),R6              ; get base address of MPACK
        MOVL    (R6),R0                 ; save NBITS
        BEQL    10$                     ; default values required ?
        MOVL    4(R6),R1                ; no, save INWORD
        BRB     20$                     ; and skip
10$:    MOVL    #1,R0                   ; yes, NBITS = 1
        MOVL    #32,R1                  ; and INWORD = 32

20$:    MOVL    @8(AP),R6               ; get byte number
        DECL    R6                      ; change it to internal counting
        DIVL3   R1,R6,R2                ; compute longword offset
        MULL3   R1,R2,R4                ; number of bytes up to offset
        SUBL3   R4,R6,R3                ; byte number
        MULL3   R0,R3,R4                ; finally, position of first bit
        MOVAL   @12(AP),R5              ; get base address of IY
        MOVAL   @4(AP)[R2],R2           ; compute base address of bit field
        MOVL    @16(AP),R6              ; get counter

30$:    EXTZV   R4,R0,(R2),(R5)+        ; extract a byte
        INCL    R3                      ; JB = JB + 1
        CMPL    R3,R1                   ; JB >= INWORD ?
        BGEQ    40$
        ADDL2   R0,R4                   ; no, just update position
        BRB     50$                     ; and skip
40$:    ADDL2   #4,R2                   ; yes, update base address
        CLRQ    R3                      ; reset position and byte number
50$:    SOBGTR  R6,30$                  ; and loop
        RET

        .END
