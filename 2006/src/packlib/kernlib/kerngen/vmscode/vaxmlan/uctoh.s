;
; $Id: uctoh.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: uctoh.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UCTOH
;
; CERN PROGLIB# M409    UCTOH           .VERSION KERNVAX  2.21  870121
; ORIG. 21/9/82, JZ, CERN
;
;     CALL UCTOH (AVC,XVM,NPW,N)     CHARACTER TO HOLLERITH AN
;
        .ENTRY UCTOH,^M<IV,R2,R3,R4,R5>
        MOVL    @16(AP),R4      ; R4 = N
        BLEQ    99$             ; --> EXIT IF N .LE. 0
        MOVAL   @4(AP),R1       ; R1 = ADR(D FOR AVC) OR ADR(AVC)
        MOVAL   @8(AP),R3       ; R3 = ADR(XVM)
        CMPB    #14,2(R1)       ; CHECK BYTE 3 IN DESCR IS =14
        BNEQ    10$
        CMPB    #8,3(R1)        ; CHECK BYTE 4 IN DESCR IS <8
        BLEQ    10$
        MOVAL   @4(R1),R1       ; R1 = ADR(AVC) FROM DESCIPTOR
10$:    MOVL    #^A/    /,R5    ; R5 = 4 BLANKS
        MOVL    @12(AP),R0      ; R0 = NPW
        CMPL    R0,#4           ; CHECK  NPW .GE. 4
        BGEQ    41$
        CMPL    R0,#2           ; CHECK  NPW = 2 OR 3
        BEQL    21$
        BGEQ    31$
;
;               NPW = 1
11$:
        MOVL    R5,R0           ; FIRST CLEAR LWORD
        MOVB    (R1)+,R0        ; AND PUT CHAR IN FIRST BYTE
        MOVL    R0,(R3)+        ; STORE LWORD
        SOBGTR  R4,11$          ; NEXT CHAR
        JMP     99$
;
;               NPW = 4
41$:
        ADDL3   #3,R4,R2        ; R2 = NPW + 3
        ASHL    #-2,R2,R2       ;              /4
        ASHL     #2,R2,R2       ;              *4
        MOVC5   R4,(R1),R5,R2,(R3)
;
99$:    RET
;
;               NPW = 2
21$:
        ASHL    #-1,R4,R2       ; R2 = R4/2
        BEQL    11$
        ASHL     #1,R2,R0
        SUBL2   R0,R4           ; R4 = 0/1 TRAILING CHAR
24$:    MOVL    R5,R0           ; FIRST CLEAR LWORD
        MOVW    (R1)+,R0        ; AND COPY 2 BYTES
        MOVL    R0,(R3)+        ; STORE LWORD
        SOBGTR  R2,24$          ; NEXT
        TSTL    R4              ; CHECK IF 1 TRAILING CHAR
        BNEQ    37$
        JMP     99$
;
;               NPW = 3
31$:
        DIVL3   #3,R4,R2        ; R2 = R4/3
        BEQL    21$
        MULL3   #3,R2,R0
        SUBL2   R0,R4           ; R4 = 0/1/2 TRAILING CHARS
34$:    MOVL    R5,R0           ; FIRST CLEAR LWORD
        MOVB    (R1)+,R0        ; AND COPY 1 BYT
        ROTL    #-8,R0,R0
        MOVW    (R1)+,R0        ; AND COPY 2 BYTES
        ROTL     #8,R0,(R3)+    ; ROTATE AND STORE LWORD
        SOBGTR  R2,34$          ; NEXT
        CMPL    R4,#1           ; CHECK IF 0/1/2 TRAILING CHAR
        BLSS    99$
        BEQL    37$
        MOVW    (R1),R5
        JMP     39$
37$:    MOVB    (R1),R5
39$:    MOVL    R5,(R3)         ; STORE INCOMPLETE WORD
        JMP     99$
        .END
