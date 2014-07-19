;
; $Id: ucopy2.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: ucopy2.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UCOPY2
;++
; CERN PROGLIB# V301    UCOPY           .VERSION KERNVAX  2.21  870121
; ORIG.  J. VORBRUEGGEN 30/4/83, CORR. 5/12/85 FOR R0=65536
;
; SUBROUTINE UCOPY(A, B, N)
; Copy source to destination array with count of longwords
;
; Register usage:
;         N <= 16383              N > 16383
; R0    byte count              (used by MOVC3)
; R1    |                       source address
; R2    |                       (used by MOVC3)
; R3    | (used by MOVC3)       destination address
; R4    |                       (used by MOVC3)
; R5    |                       (used by MOVC3)
; R6    not used                = 65535
; R7    not used                updated byte count
;
; NOTE:  - Entries UCOPY and UCOPY2 are equivalent, as the
;          VAX hardware takes care of overlap in most cases.
;        - R6/R7 are only saved if necessary (i. e. N > 16383)
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UCOPY2,^M<R2,R3,R4,R5>
        BRB     GOON
        .ENTRY  UCOPY,^M<R2,R3,R4,R5>
GOON:   NOP

        ASHL    #2,@12(AP),R0           ; Make count of bytes (*4)
        BLEQ    10$                     ; Return if <= 0
        CMPL    #65535,R0               ; Too large for one MOVC3 ?
        BLSSU   20$
        MOVC3   R0,@4(AP),@8(AP)        ; Normal case.
10$:    RET

20$:    MOVQ    R6,-(SP)                ; Fastest way of saving R6/R7
        MOVAL   @4(AP),R1               ; Get address of source
        MOVL    #65535,R6               ; Max. # of bytes for MOVC3
        MOVL    R0,R7                   ; Save count from destruction
        MOVAL   @8(AP),R3               ; Get address of destination
        CMPL    R3,R1                   ; Check relative position of
                                        ; source and destination
        BEQL    10$                     ; Nothing to do.
        BGTRU   50$                     ; Special case - do move from end

30$:    MOVC3   R6,(R1),(R3)            ; Move a 64KB bite
        ACBL    R6,#-65535,R7,30$       ; Any more ?
        BEQL    40$                     ; Already finished ?
        MOVC3   R7,(R1),(R3)            ; Move the rest
40$:    MOVQ    (SP)+,R6                ; Restore registers
        RET

50$:    SUBL2   R6,R7                   ; Set pointer to first bite

60$:    ADDL3   R7,4(AP),R1             ; Adjust source address
        ADDL3   R7,8(AP),R3             ; and destination address
        MOVC3   R6,(R1),(R3)            ; Move a 64KB bite
        ACBL    #0,#-65535,R7,60$       ; Any more ?
        ADDL2   R6,R7                   ; Correct counter
        MOVC3   R7,@4(AP),@8(AP)        ; Move the last bite
        MOVQ    (SP)+,R6                ; Restore registers
        RET                             ; Done.
        .END
