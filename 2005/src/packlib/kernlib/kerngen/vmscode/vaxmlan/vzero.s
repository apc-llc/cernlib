;
; $Id: vzero.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: vzero.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE VZERO
;++
; CERN PROGLIB# F121    VZERO           .VERSION KERNVAX  2.18  860429
; ORIG. J. VORBRUEGGEN 30/4/83
;
; SUBROUTINE VZERO(X, N)
; Preset array to zero with count of longwords
;
; Register usage:
;         N <= 16383              N > 16383
; R0    byte count              (used by MOVC5)
; R1    dummy source address    dummy source address
; R2    |                       (used by MOVC5)
; R3    | (used by MOVC5)       destination address
; R4    |                       (used by MOVC5)
; R5    |                       (used by MOVC5)
; R6    not used                = 65535
; R7    not used                updated byte count
;
; NOTE: R6/R7 are only saved if necessary (i. e. N > 16383)
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  VZERO,^M<R2,R3,R4,R5>

        ASHL    #2,@8(AP),R0            ; Make byte count (*4)
        BLEQ    10$                     ; Return if <= 0
        CMPL    #65535,R0               ; Too large for one MOVC5 ?
        BLSSU   20$
        MOVC5   #0,(R1),#0,R0,@4(AP)    ; Normal case.
10$:    RET

20$:    MOVQ    R6,-(SP)                ; Fastest way of saving R6/R7
        MOVAL   @4(AP),R3               ; Get address of source
        MOVL    #65535,R6               ; Max. # of bytes for MOVC5
        MOVL    R0,R7                   ; Save count from destruction

30$:    MOVC5   #0,(R1),#0,R6,(R3)      ; Zero one 64KB chunk
        ACBL    R6,#-65535,R7,30$       ; Any more ?
        BEQL    40$                     ; Already finished ?
        MOVC5   #0,(R1),#0,R7,(R3)      ; No, zero the rest
40$:    MOVQ    (SP)+,R6                ; Restore registers
        RET
        .END
