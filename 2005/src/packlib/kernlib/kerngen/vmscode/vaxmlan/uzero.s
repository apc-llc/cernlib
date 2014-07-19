;
; $Id: uzero.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: uzero.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UZERO
;++
; CERN PROGLIB# V300    UZERO           .VERSION KERNVAX  2.18  860429
; ORIG.  J. VORBRUEGGEN 30/4/83
;
; SUBROUTINE UZERO(A, JL, JN)
; Zero array from element JL to JN
;
; Register usage:
;         JN-JL <= 16383                  JN-JL > 16383
; R0    byte offset to A(JL) from A(1)  (used by MOVC5)
; R1    byte count                      dummy source address
; R2    dummy source address            (used by MOVC5)
; R3    |                               destination address
; R4    | (used by MOVC5)               (used by MOVC5)
; R5    |                               (used by MOVC5)
; R6    not used                        = 65535
; R7    not used                        updated byte count
;
; NOTE: R6/R7 are only saved if needed (i. e. JN-JL > 16383)
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UZERO,^M<R2,R3,R4,R5>

        ASHL    #2,@8(AP),R0            ; Compute byte offset to A(JL)
        SUBL2   #4,R0
        ASHL    #2,@12(AP),R1           ; Compute byte offset to A(JN+1)
        SUBL2   R0,R1                   ; # of bytes to zero
        CMPL    #65535,R1               ; Too large for MOVC5 ?
        BLSSU   10$
        MOVC5   #0,(R2),#0,R1,@4(AP)[R0]; Normal case.
        RET

10$:    MOVQ    R6,-(SP)                ; Fastest way of saving R6/R7
        MOVL    #65535,R6               ; Max. # of bytes for MOVC5
        ADDL3   4(AP),R0,R3             ; Compute address of A(JL)
        MOVL    R1,R7                   ; Save count from destruction
20$:    MOVC5   #0,(R1),#0,R6,(R3)      ; Zero one 64KB chunk
        ACBL    R6,#-65535,R7,20$       ; Any more ?
        BEQL    30$                     ; Already finished ?
        MOVC5   #0,(R1),#0,R7,(R3)      ; No, zero the rest
30$:    MOVQ    (SP)+,R6                ; Restore registers
        RET
        .END
