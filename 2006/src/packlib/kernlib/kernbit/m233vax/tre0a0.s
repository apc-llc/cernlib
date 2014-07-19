;
; $Id: tre0a0.s,v 1.1.1.1 1996/02/15 17:47:47 mclareni Exp $
;
; $Log: tre0a0.s,v $
; Revision 1.1.1.1  1996/02/15 17:47:47  mclareni
; Kernlib
;
;
 .TITLE TRE0A0
;++
; CERN PROGLIB# M233    TRE0A0                  .VERSION KERNVAX 2.01
; ORIG.  J. VORBRUEGGEN 28/5/83
;
; SUBROUTINE TRE0A0(A, N)
; Translate the N characters at A from CERN EBCDIC to CERN ASCII
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
        .ENTRY  TRE0A0,^M<R2,R3,R4,R5>

        MOVL    @8(AP),R0               ; get N
        BLEQ    10$                     ; return if <= 0
        CMPL    #65536,R0               ; can we do it in one go ?
        BLSSU   20$                     ; if more than 64KB, use loop
        MOVTC   R0,@4(AP),#0,E0A0TABLE,R0,@4(AP) ; Normal case.
10$:    RET

20$:    MOVQ    R6,-(SP)                ; first save two registers
        MOVL    R0,R7                   ; save count from destruction
        MOVAL   @4(AP),R1               ; get base address of A
        MOVL    R1,R5                   ; copy it for use as destination
        MOVAL   E0A0TABLE,R3            ; get address of translation table
        MOVL    #65535,R6               ; this is the maximum for one MOVTC

30$:    MOVTC   R6,(R1),#0,(R3),R6,(R5) ; translate one 64KB chunk
        ACBL    R6,#-65535,R7,30$       ; any more ?
        BEQL    40$                     ; already finished ?
        MOVTC   R7,(R1),#0,(R3),R7,(R5) ; no, translate the rest
40$:    MOVQ    (SP)+,R6                ; restore registers
        RET

        .PSECT  $PDATA,PIC,CON,REL,LCL,SHR,NOEXE,RD,NOWRT,LONG
;++
; This translation table converts CERN EBCDIC to CERN ASCII.
; It is based on the Rutherford table with the following
; modifications:
;  - EBCDIC 6A is converted to ASCII 5E (circumflex)
;  - EBCDIC 71 is considered illegal
;  - illegal characters are converted to 23 (hash) rather than
;    25 (percent)
;
; Modification Log
;
; Change in the CERN IBM character set - January 1990
;
;   braces     C0/D0 map to ASCII 7B/7D (mapped to 23 hash before)
;   8B/9B      map to 23 hash (illegal) (mapped to braces before)
;   tilde      A1 maps to ASCII 7E tilde (mapped to 23 hash before)
;   not sign   5F maps to ASCII 5E caret (mapped to 7E tilde before)
;   split bar  6A maps to ASCII 23 hash (mapped to 5E caret before)
;   caret      53 maps to ASCII 23 hash (unchanged !)
;
;--
E0A0TABLE:
        .BYTE   ^X00,^X01,^X02,^X03,^X23,^X09,^X23,^X7F  ; 07
        .BYTE   ^X23,^X23,^X23,^X0B,^X0C,^X0D,^X0E,^X0F  ; 0F
        .BYTE   ^X10,^X11,^X12,^X13,^X23,^X23,^X08,^X23  ; 17
        .BYTE   ^X18,^X19,^X23,^X23,^X1C,^X1D,^X1E,^X1F  ; 1F
        .BYTE   ^X23,^X23,^X23,^X23,^X23,^X0A,^X17,^X1B  ; 27
        .BYTE   ^X23,^X23,^X23,^X23,^X23,^X05,^X06,^X07  ; 2F
        .BYTE   ^X23,^X23,^X16,^X23,^X23,^X23,^X23,^X04  ; 37
        .BYTE   ^X23,^X23,^X23,^X23,^X14,^X15,^X23,^X1A  ; 3F
        .BYTE   ^X20,^X23,^X23,^X23,^X23,^X23,^X23,^X23  ; 47
        .BYTE   ^X23,^X23,^X23,^X2E,^X3C,^X28,^X2B,^X7C  ; 4F
        .BYTE   ^X26,^X23,^X23,^X23,^X23,^X23,^X23,^X23  ; 57
        .BYTE   ^X23,^X23,^X21,^X24,^X2A,^X29,^X3B,^X5E  ; 5F
;       .BYTE   ^X23,^X23,^X21,^X24,^X2A,^X29,^X3B,^X7E  ; Change 5F 7E->5E
        .BYTE   ^X2D,^X2F,^X23,^X23,^X23,^X23,^X23,^X23  ; 67
        .BYTE   ^X23,^X23,^X23,^X2C,^X25,^X5F,^X3E,^X3F  ; 6F
;       .BYTE   ^X23,^X23,^X5E,^X2C,^X25,^X5F,^X3E,^X3F  ; Change 6A 5E->23
        .BYTE   ^X23,^X23,^X23,^X23,^X23,^X23,^X23,^X23  ; 77
        .BYTE   ^X23,^X60,^X3A,^X23,^X40,^X27,^X3D,^X22  ; 7F
        .BYTE   ^X23,^X61,^X62,^X63,^X64,^X65,^X66,^X67  ; 87
        .BYTE   ^X68,^X69,^X23,^X23,^X23,^X23,^X23,^X23  ; 8F
;       .BYTE   ^X68,^X69,^X23,^X7B,^X23,^X23,^X23,^X23  ; Change 8B 7B->23
        .BYTE   ^X23,^X6A,^X6B,^X6C,^X6D,^X6E,^X6F,^X70  ; 97
        .BYTE   ^X71,^X72,^X23,^X23,^X23,^X23,^X23,^X23  ; 9F
;       .BYTE   ^X71,^X72,^X23,^X7D,^X23,^X23,^X23,^X23  ; Change 9B 7D->23
        .BYTE   ^X23,^X7E,^X73,^X74,^X75,^X76,^X77,^X78  ; A7
;       .BYTE   ^X23,^X23,^X73,^X74,^X75,^X76,^X77,^X78  ; Change A1 23->7E
        .BYTE   ^X79,^X7A,^X23,^X23,^X23,^X5B,^X23,^X23  ; AF
        .BYTE   ^X23,^X23,^X23,^X23,^X23,^X23,^X23,^X23  ; B7
        .BYTE   ^X23,^X23,^X23,^X23,^X23,^X5D,^X23,^X23  ; BF
        .BYTE   ^X7B,^X41,^X42,^X43,^X44,^X45,^X46,^X47  ; C7
;       .BYTE   ^X23,^X41,^X42,^X43,^X44,^X45,^X46,^X47  ; Change C0 23->7B
        .BYTE   ^X48,^X49,^X23,^X23,^X23,^X23,^X23,^X23  ; CF
        .BYTE   ^X7D,^X4A,^X4B,^X4C,^X4D,^X4E,^X4F,^X50  ; D7
;       .BYTE   ^X23,^X4A,^X4B,^X4C,^X4D,^X4E,^X4F,^X50  ; Change D0 23->7D
        .BYTE   ^X51,^X52,^X23,^X23,^X23,^X23,^X23,^X23  ; DF
        .BYTE   ^X5C,^X23,^X53,^X54,^X55,^X56,^X57,^X58  ; E7
        .BYTE   ^X59,^X5A,^X23,^X23,^X23,^X23,^X23,^X23  ; EF
        .BYTE   ^X30,^X31,^X32,^X33,^X34,^X35,^X36,^X37  ; F7
        .BYTE   ^X38,^X39,^X23,^X23,^X23,^X23,^X23,^X23  ; FF
  .END
