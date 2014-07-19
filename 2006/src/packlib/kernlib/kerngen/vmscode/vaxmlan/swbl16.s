;
; $Id: swbl16.s,v 1.1.1.1 1996/02/15 17:50:30 mclareni Exp $
;
; $Log: swbl16.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:30  mclareni
; Kernlib
;
;
 .TITLE SWBL16
;++
; CERN PROGLIB#         SWBLOW16        .VERSION KERNVAX  1.08  810129
; ORIG.  E. PAGIOLA     28/1/81
; MOD.   J. VORBRUEGGEN 31/5/83
;
; SUBROUTINE VAX_SWBLOW16(RECORD, WORDS, N16)
; Blow the raw data in RECORD, 4 bytes per longword,
; into the N16 16-bit words in WORDS with bytes swopped.
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VAX_SWBLOW16,^M<R2,R3>

        MOVQ    4(AP),R0                ;INPUT AND OUTPUT BUFFER ADR
        MOVL    @12(AP),R2              ;R2=COUNT

10$:    MOVB    (R0)+,R3                ;FIRST BYTE
        ROTL    #8,R3,R3                ;TO 2ND BYTE
        MOVB    (R0)+,R3                ;2ND BYTE TO FIRST
        MOVZWL  R3,(R1)+                ;STORE  16 BIT WORD
        SOBGTR  R2,10$                  ;LOOP
        RET
        .END
