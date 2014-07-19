;
; $Id: upkch.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: upkch.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UPKCH
;++
; CERN PROGLIB# M427    UPKCH           .VERSION KERNVAX  2.32  890926
; ORIG.  J. VORBRUEGGEN 18/5/83
; MOD. JV 8/10/83, FCA 26/09/89
;
; SUBROUTINE UPKCH(CHAR, INT, N, IPAR)
; Unpack the N bytes in CHAR into the array INT.
; Bytes are described by IPAR in the following way:
;
; IPAR(1)       (= NBITS) # of bits per byte
; IPAR(2)       (= NCHAR) # of bytes per zone; if zero, maximum is used
; IPAR(3)       (= NZONE) # of bits per zone; if zero, set to 32
; IPAR(4)       (= LBITS) # of leading bits of each zone to be ignored
;
; Bits are numbered from left to right, starting with 1;
; bytes crossing word boundaries are discontinuous!
; Because of this, NZONE = 32 is treated seperately.
; If the specified number of bytes won't fit into a field
; of length NZONE-LBITS, NCHAR is set to the maximum that will.
; On return, the first longword of COMMON /SLATE/ contains the number
; of longwords used in CHAR, not including trailing bits of last zone.
;
; Register usage:
; R0    counter from N to 1
; R1    counter from NCHAR to 1
; R2    = NZONE
; R3    = NBITS
; R4    = LBITS
; R5    = NCHAR (temporarily used for address of IPAR)
; R6    address of CHAR(NWORD)
; R7    address of next integer to receive a byte
; R8    position of LSB of current byte
;
; If NZONE = 32,
; R9    position of LSB of first byte in longword
; R10   not used
; R11   not used
;
; If NZONE <> 32,
; R9    if byte spans longwords, contains # of bits left for this word
; R10   if byte spans longwords, contains # of bits for next longword
; R11   # of bits to be skipped between two zones
;
; R9 is used as scratch register for calculations in both cases.
; R10/R11 are saved only if needed (i. e. NZONE <> 32).
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UPKCH,^M<R2,R3,R4,R5,R6,R7,R8,R9>

        MOVL    @12(AP),R0              ; get counter
        BGTR    10$                     ; continue if > 0
        RET                             ; else return
10$:    MOVAL   @16(AP),R5              ; get base address of IPAR
        MOVAL   @4(AP),R6               ; get base address of CHAR
        MOVAL   @8(AP),R7               ; and INT
        MOVL    8(R5),R2                ; get NZONE
        BNEQ    20$                     ; default needed ?
        MOVL    #32,R2                  ; yes, NZONE = 32
20$:    MOVL    (R5),R3                 ; get NBITS
        MOVL    12(R5),R4               ; and LBITS
        MOVL    4(R5),R5                ; and NCHAR
        BEQL    30$                     ; default needed ?
        MULL3   R3,R5,R9                ; no, check for valid arguments
        ADDL2   R4,R9                   ; compute NBITS*NCHAR+LBITS
        CMPL    R9,R2                   ; do they fit into NZONE bits ?
        BLEQ    40$                     ; if not, set NCHAR to maximum
30$:    SUBL3   R4,R2,R9                ; compute # of usable bits
        DIVL3   R3,R9,R5                ; divide result by size of byte
40$:    MOVL    #1,NWORD                ; initialize NWORD
        SUBL3   R4,#32,R8               ; compute pointer to end of 1st byte
        CMPL    #32,R2                  ; zone of one longword ?
        BNEQ    NOTWRD
                                ; we get here if that's so
WORD:   SUBL2   R3,R8                   ; compute position of 1st byte in zone
        MOVL    R8,R9                   ; save it for later use
        BRB     30$                     ; start the loop correctly

10$:    DECL    R1                      ; any more byes for this zone ?
        BEQL    20$
        SUBL2   R3,R8                   ; yes, update pointer
        EXTZV   R8,R3,(R6),(R7)+        ; unpack one byte
        SOBGTR  R0,10$                  ; and loop
        RET
                                        ; new zone:
20$:    ADDL2   #4,R6                   ; update address in CHAR
        INCL    NWORD                   ; count the longword
        MOVL    R9,R8                   ; set up position of 1st byte
30$:    MOVL    R5,R1                   ; initialize byte counter
        EXTZV   R8,R3,(R6),(R7)+        ; unpack one byte | repeated
        SOBGTR  R0,10$                  ; and loop        | for speed
        RET
                                ; we get here if everything's possible
NOTWRD: MOVQ    R10,-(SP)               ; save the two registers
        MULL3   R3,R5,R9                ; compute # of bits used by bytes
        SUBL3   R9,R2,R11               ; compute # of bits between zones

10$:    MOVL    R5,R1                   ; initialize byte counter
        TSTL    R8                      ; position negative <=> new longword ?
        BGEQ    30$
20$:    ADDL2   #4,R6                   ; yes, update address in CHAR
        INCL    NWORD                   ; and take account of that
        ADDL2   #32,R8                  ; correct our position
        BLSS    20$                     ; another longword to skip ?

30$:    DECL    R1                      ; any more bytes for this zone ?
        BLSS    40$
        SUBL2   R3,R8                   ; yes, update position
        BLSS    50$                     ; does byte span longwords ?
        EXTZV   R8,R3,(R6),(R7)+        ; no, unpack one byte
        SOBGTR  R0,30$                  ; and loop
        MOVQ    (SP)+,R10               ; restore registers
        RET

40$:    SUBL2   R11,R8                  ; skip trailing and leading bits
        BRB     10$                     ; and start with new zone

50$:    MOVL    R8,R9                   ; save position
        MNEGL   R8,R10                  ; compute # of bits in next longword
        ADDL2   #32,R8                  ; correct position
        ADDL2   #4,R6                   ; update address in CHAR
        INCL    NWORD                   ; update NWORD
        EXTZV   R8,R10,(R6),(R7)+       ; extract lower part of byte
        ADDL2   R3,R9                   ; any bits left ?
        BEQL    60$
        INSV    -4(R6),R10,R9,-4(R7)    ; yes, insert them
60$:    SOBGTR  R0,30$                  ; loop
        MOVQ    (SP)+,R10               ; restore registers
        RET

        .PSECT  SLATE,PIC,OVR,REL,GBL,SHR,NOEXE,RD,WRT,LONG

NWORD:  .BLKL   1
RESERV: .BLKL   39

        .END
