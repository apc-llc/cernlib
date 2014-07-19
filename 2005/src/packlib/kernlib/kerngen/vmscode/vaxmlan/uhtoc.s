;
; $Id: uhtoc.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: uhtoc.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UHTOC
;
; CERN PROGLIB# M409    UHTOC           .VERSION KERNVAX  2.21  870121
; ORIG. 22/9/82, JZ, CERN
;
;     CALL UHTOC (AVM,NPW,XVC,N)     HOLLERITH AN TO CHARACTER
;
        .ENTRY UHTOC,^M<IV,R2,R3,R4,R5>
        MOVL    @16(AP),R4      ; R4 = N
        BLEQ    99$             ; --> EXIT IF N .LE. 0
        MOVAL   @12(AP),R3      ; R3 = ADR(D FOR XVC) OR ADR(XVC)
        MOVAL   @4(AP),R1       ; R1 = ADR(AVM)
        CMPB    #14,2(R3)       ; CHECK BYTE 3 IN DESCR IS =14
        BNEQ    10$
        CMPB    #8,3(R3)        ; CHECK BYTE 4 IN DESCR IS <8
        BLEQ    10$
        MOVAL   @4(R3),R3       ; R3 = ADR(XVC) FROM DESCIPTOR
10$:    MOVL    @8(AP),R0       ; R0 = NPW
        CMPL    R0,#4           ; CHECK  NPW .GE. 4
        BGEQ    41$
        CMPL    R0,#2           ; CHECK  NPW = 2 OR 3
        BEQL    21$
        BGEQ    31$
;
;               NPW = 1
11$:
        MOVB    (R1),(R3)+      ; COPY 1 BYTE
        ADDL2   #4,R1           ; BUMP SOURCE ADR
        SOBGTR  R4,11$          ; NEXT CHAR
        BRB     99$
;
;               NPW = 4
41$:
        MOVC3   R4,(R1),(R3)
;
99$:    RET
;
;               NPW = 2
21$:
        ASHL    #-1,R4,R2       ; R2 = R4/2
        BEQL    11$
        ASHL     #1,R2,R0
        SUBL2   R0,R4           ; R4 = 0/1 TRAILING CHAR
24$:    MOVW    (R1),(R3)+      ; COPY 2 BYTES
        ADDL2   #4,R1           ; BUMP SOURCE ADR
        SOBGTR  R2,24$          ; NEXT
        TSTL    R4              ; CHECK IF 1 TRAILING CHAR
        BNEQ    11$
        JMP     99$
;
;               NPW = 3
31$:
        DIVL3   #3,R4,R2        ; R2 = R4/3
        BEQL    21$
        MULL3   #3,R2,R0
        SUBL2   R0,R4           ; R4 = 0/1/2 TRAILING CHARS
34$:    MOVB    (R1)+,(R3)+     ; COPY 1 BYT
        MOVW    (R1)+,(R3)+     ; AND COPY 2 BYTES
        INCL    R1              ; BUMP SOURCE ADR
        SOBGTR  R2,34$          ; NEXT
        TSTL    R4              ; CHECK IF TRAILING CHARS
        BNEQ    41$
        JMP     99$
        .END
