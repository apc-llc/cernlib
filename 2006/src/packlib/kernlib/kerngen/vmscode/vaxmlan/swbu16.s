;
; $Id: swbu16.s,v 1.1.1.1 1996/02/15 17:50:30 mclareni Exp $
;
; $Log: swbu16.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:30  mclareni
; Kernlib
;
;
 .TITLE SWBU16
;++
; CERN PROGLIB#         SWBUNCH16       .VERSION KERNVAX  1.08  810129
; ORIG.  E. PAGIOLA     28/1/81
; MOD.   J. VORBRUEGGEN 31/5/83
;
; SUBROUTINE VAX_SWBUNCH16(WORDS, RECORD, N16)
; Bunch the  N16 16-bit words at WORDS with byte swopping to
; the raw data in RECORD, 4 bytes per longword.
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VAX_SWBUNCH16,^M<R2,R3>

        MOVQ    4(AP),R0                ;INPUT AND OUTPUT BUFFER ADR
        MOVL    @12(AP),R2              ;R2=COUNT

10$:    MOVB    (R0),R3                 ;FIRST BYTE
        ROTL    #8,R3,R3                ;TO 2ND
        MOVB    1(R0),R3                ;2ND TO FIRST
        MOVW    R3,(R1)+                ;STORE  NEXT 2 BYTES
        ADDL2   #4,R0                   ;BUMP INPUT ADR
        SOBGTR  R2,10$
        RET
        .END
