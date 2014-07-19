;
; $Id: tra0e0.s,v 1.1.1.1 1996/02/15 17:47:48 mclareni Exp $
;
; $Log: tra0e0.s,v $
; Revision 1.1.1.1  1996/02/15 17:47:48  mclareni
; Kernlib
;
;
 .TITLE TRA0E0
;++
; CERN PROGLIB# M233    TRA0E0                  .VERSION KERNVAX 2.01
; ORIG.  J. VORBRUEGGEN 28/5/83
;
; SUBROUTINE TRA0E0(A, N)
; Translate the N characters at A from CERN ASCII to CERN EBCDIC
;
; Register usage:
;         N <= 65535              N > 65535
; R0    byte count              (used by MOVTC)
; R1    |                       source address in A
; R2    |                       (used by MOVTC)
; R3    | (used by MOVTC)       address of translation table
; R4    |                       (used by MOVTC)
; R5    |                       destination address in A (same as R1)
; R6    not used                = 65535
; R7    not used                updated byte count
;
; NOTE: R6/R7 are only saved if necessary (i. e. N > 65535).
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  TRA0E0,^M<R2,R3,R4,R5>

        MOVL    @8(AP),R0               ; get N
        BLEQ    10$                     ; return if <= 0
        CMPL    #65536,R0               ; can we do it in one go ?
        BLSSU   20$                     ; if more than 64KB, use loop
        MOVTC   R0,@4(AP),#0,A0E0TABLE,R0,@4(AP) ; Normal case.
10$:    RET

20$:    MOVQ    R6,-(SP)                ; first save two registers
        MOVL    R0,R7                   ; save count from destruction
        MOVAL   @4(AP),R1               ; get base address of A
        MOVL    R1,R5                   ; copy it for use as destination
        MOVAL   A0E0TABLE,R3            ; get address of translation table
        MOVL    #65535,R6               ; this is the maximum for one MOVTC

30$:    MOVTC   R6,(R1),#0,(R3),R6,(R5) ; translate one 64KB chunk
        ACBL    R6,#-65535,R7,30$       ; any more ?
        BEQL    40$                     ; already finished ?
        MOVTC   R7,(R1),#0,(R3),R7,(R5) ; no, translate the rest
40$:    MOVQ    (SP)+,R6                ; restore registers
        RET

        .PSECT  $PDATA,PIC,CON,REL,LCL,SHR,NOEXE,RD,NOWRT,LONG
;++
; This translation table converts CERN ASCII to CERN EBCDIC.
; It is based on the Rutherford table with the following
; modifications:
;  - ASCII 5E is converted to EBCDIC 6A (circumflex)
;  - illegal characters are converted to 7B (hash)
;    rather than 6C (percent)
;
; Modification Log
;
; Change in the CERN IBM character set - January 1990
;
;   braces     7B/7D map to EBCDIC C0/D0 (mapped to 8B/9B before)
;   tilde      7E maps to EBCDIC A1 (mapped to 5F before)
;   caret      5E maps to EBCDIC 5F (mapped to 6A before)
;--
A0E0TABLE:
        .BYTE   ^X00,^X01,^X02,^X03,^X37,^X2D,^X2E,^X2F ; 07
        .BYTE   ^X16,^X05,^X25,^X0B,^X0C,^X0D,^X0E,^X0F ; 0F
        .BYTE   ^X10,^X11,^X12,^X13,^X3C,^X3D,^X32,^X26 ; 17
        .BYTE   ^X18,^X19,^X3F,^X27,^X1C,^X1D,^X1E,^X1F ; 1F
        .BYTE   ^X40,^X5A,^X7F,^X7B,^X5B,^X6C,^X50,^X7D ; 27
        .BYTE   ^X4D,^X5D,^X5C,^X4E,^X6B,^X60,^X4B,^X61 ; 2F
        .BYTE   ^XF0,^XF1,^XF2,^XF3,^XF4,^XF5,^XF6,^XF7 ; 37
        .BYTE   ^XF8,^XF9,^X7A,^X5E,^X4C,^X7E,^X6E,^X6F ; 3F
        .BYTE   ^X7C,^XC1,^XC2,^XC3,^XC4,^XC5,^XC6,^XC7 ; 47
        .BYTE   ^XC8,^XC9,^XD1,^XD2,^XD3,^XD4,^XD5,^XD6 ; 4F
        .BYTE   ^XD7,^XD8,^XD9,^XE2,^XE3,^XE4,^XE5,^XE6 ; 57
        .BYTE   ^XE7,^XE8,^XE9,^XAD,^XE0,^XBD,^X5F,^X6D ; 5F
;       .BYTE   ^XE7,^XE8,^XE9,^XAD,^XE0,^XBD,^X6A,^X6D ; Change 5E 6A->5F
        .BYTE   ^X79,^X81,^X82,^X83,^X84,^X85,^X86,^X87 ; 67
        .BYTE   ^X88,^X89,^X91,^X92,^X93,^X94,^X95,^X96 ; 6F
        .BYTE   ^X97,^X98,^X99,^XA2,^XA3,^XA4,^XA5,^XA6 ; 77
        .BYTE   ^XA7,^XA8,^XA9,^XC0,^X4F,^XD0,^XA1,^X07 ; 7F
;       .BYTE   ^XA7,^XA8,^XA9,^X8B,^X4F,^X9B,^X5F,^X07
;                                            Change 7B/7D/7E 8B/9B/5F->C0/D0/A1
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; 87
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; 8F
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; 97
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; 9F
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; A7
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; AF
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; B7
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; BF
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; C7
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; CF
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; D7
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; DF
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; E7
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; EF
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; F7
        .BYTE   ^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B,^X7B ; FF
  .END
