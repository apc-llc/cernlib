;
; $Id: pkchar.s,v 1.1.1.1 1996/02/15 17:50:28 mclareni Exp $
;
; $Log: pkchar.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:28  mclareni
; Kernlib
;
;
 .TITLE PKCHAR
;++
; CERN PROGLIB# M427    PKCHAR          .VERSION KERNVAX  2.32  890926
; ORIG.  J. VORBRUEGGEN 17/5/83
; MOD. JV 8/10/83, FCA 26/09/89
;
; SUBROUTINE PKCHAR(INT, CHAR, N, IPAR)
; Pack the N small integers in array INT into CHAR
; as bytes described by IPAR in the following way:
;
; IPAR(1)       (= NBITS) # of bits per byte
; IPAR(2)       (= NCHAR) # of bytes per zone; if zero, maximum is used
; IPAR(3)       (= NZONE) # of bits per zone; if zero, set to 32
; IPAR(4)       (= LBITS) # of leading bits of each zone to be ignored
; IPAR(5)       (= IFILL) longword with which each word used is preset
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
; R5    = NCHAR
; R6    = IFILL (temporarily used for address of IPAR)
; R7    address of CHAR(NWORD)
; R8    address of next integer to be packed
; R9    position of LSB of current byte
;
; If NZONE = 32,
; R10   position of LSB of first byte in longword
; R11   not used
;
; If NZONE <> 32,
; R10   if byte spans longwords, contains # of bits left for this word
; R11   if byte spans longwords, contains part of byte to be packed
; NSKIP # of bits to be skipped between two zones
;
; R10 is used as scratch register for calculations in both cases.
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  PKCHAR,^M<R2,R3,R4,R5,R6,R7,R8,R9,R10,R11>

        MOVL    @12(AP),R0              ; get counter
        BGTR    10$                     ; continue if > 0
        RET                             ; else return
10$:    MOVAL   @16(AP),R6              ; get base address of IPAR
        MOVAL   @8(AP),R7               ; get base address of CHAR
        MOVAL   @4(AP),R8               ; and INT
        MOVL    8(R6),R2                ; get NZONE
        BNEQ    20$                     ; default needed ?
        MOVL    #32,R2                  ; yes, NZONE = 32
20$:    MOVL    (R6),R3                 ; get NBITS
        MOVL    12(R6),R4               ; and LBITS
        MOVL    4(R6),R5                ; and NCHAR
        BEQL    30$                     ; default needed ?
        MULL3   R3,R5,R11               ; no, check for valid arguments
        ADDL2   R4,R11                  ; compute NBITS*NCHAR+LBITS
        CMPL    R11,R2                  ; do they fit into NZONE bits ?
        BLEQ    40$                     ; if not, set NCHAR to maximum
30$:    SUBL3   R4,R2,R11               ; compute # of usable bits
        DIVL3   R3,R11,R5               ; divide result by size of byte
40$:    MOVL    16(R6),R6               ; get IFILL
        MOVL    R6,(R7)                 ; preset first longword of CHAR
        MOVL    #1,NWORD                ; and initialize NWORD
        SUBL3   R4,#32,R9               ; compute pointer to end of 1st byte
        CMPL    #32,R2                  ; zone of one longword ?
        BNEQ    NOTWRD
                                ; we get here if that's so
WORD:   SUBL2   R3,R9                   ; compute position of 1st byte in zone
        MOVL    R9,R10                  ; save it for later use
        BRB     30$                     ; start the loop correctly

10$:    DECL    R1                      ; any more bytes for this zone ?
        BEQL    20$
        SUBL2   R3,R9                   ; yes, update pointer
        INSV    (R8)+,R9,R3,(R7)        ; pack one byte
        SOBGTR  R0,10$                  ; and loop
        RET
                                        ; new zone:
20$:    ADDL2   #4,R7                   ; update address in CHAR
        MOVL    R10,R9                  ; set position pointer to 1st byte
        MOVL    R6,(R7)                 ; preset the new longword
        INCL    NWORD                   ; and count it
30$:    MOVL    R5,R1                   ; initialize byte counter
        INSV    (R8)+,R9,R3,(R7)        ; pack one byte | repeated
        SOBGTR  R0,10$                  ; and loop      | for speed
        RET
                                ; we get here if everything's possible
NOTWRD: MULL3   R3,R5,R10               ; compute # of bits used by bytes
        SUBL3   R10,R2,NSKIP            ; compute # of bits between zones

10$:    MOVL    R5,R1                   ; initialize byte counter
        TSTL    R9                      ; position negative <=> new longword ?
        BGEQ    30$
20$:    ADDL2   #4,R7                   ; yes, update address in CHAR
        MOVL    R6,(R7)                 ; fill it
        INCL    NWORD                   ; and take account of that
        ADDL2   #32,R9                  ; correct our position
        BLSS    20$                     ; another longword to fill ?

30$:    DECL    R1                      ; any more bytes for this zone ?
        BLSS    40$
        SUBL2   R3,R9                   ; yes, update position
        BLSS    50$                     ; does byte span longwords ?
        INSV    (R8)+,R9,R3,(R7)        ; no, pack it
        SOBGTR  R0,30$                  ; and loop
        RET

40$:    SUBL2   NSKIP,R9                ; skip trailing and leading bits
        BRB     10$                     ; and start new zone

50$:    ADDL3   R3,R9,R10               ; compute # of bits left
        BEQL    60$                     ; none ?
        ASHL    R9,(R8),R11             ; save EXTZV by shifting source
        INSV    R11,#0,R10,(R7)         ; pack the upper bits which fit
60$:    ADDL2   #4,R7                   ; update address in CHAR
        MOVL    R6,(R7)                 ; fill the new longword
        INCL    NWORD                   ; and remember that we did
        MNEGL   R9,R11                  ; # of bytes left to pack
        ADDL2   #32,R9                  ; update position
        INSV    (R8)+,R9,R11,(R7)       ; and pack the rest
        SOBGTR  R0,30$                  ; loop
        RET

        .PSECT  SLATE,PIC,OVR,REL,GBL,SHR,NOEXE,RD,WRT,LONG

NWORD:  .BLKL   1
NSKIP:  .BLKL   1
RESERV: .BLKL   38

        .END
