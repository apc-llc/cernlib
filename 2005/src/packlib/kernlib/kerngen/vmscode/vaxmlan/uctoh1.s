;
; $Id: uctoh1.s,v 1.1.1.1 1996/02/15 17:50:31 mclareni Exp $
;
; $Log: uctoh1.s,v $
; Revision 1.1.1.1  1996/02/15 17:50:31  mclareni
; Kernlib
;
;
 .TITLE UCTOH1
;++
; CERN PROGLIB# M409    UCTOH1          .VERSION KERNVAX  1.13  820606
; ORIG. 4/6/82, JZ, CERN
; MOD.  J.VORBRUEGGEN   31/5/83
;
; SUBROUTINE UCTOH1(AVC, XVM, N)
; Convert the N bytes in AVC to XVM in 1H format;
; check first whether AVC is pssed by descriptor
; (real CHARACTER data) or by reference (anything else).
;--
        .IDENT  /01/
        .PSECT  $CODE,PIC,CON,REL,LCL,SHR,EXE,RD,NOWRT,LONG
        .ENTRY  UCTOH1,^M<R2,R3>

        MOVL    @12(AP),R0              ; R0 = N
        BLEQ    99$                     ; --> EXIT IF N .LE. 0
        MOVAL   @4(AP),R2               ; R2 = ADR(D FOR AVC) OR ADR(AVC)
        MOVAL   @8(AP),R3               ; R3 = ADR(XVM)
        CMPB    #14,2(R2)               ; CHECK BYTE 3 IN DESCR IS =14
        BNEQ    24$
        CMPB    #8,3(R2)                ; CHECK BYTE 4 IN DESCR IS <8
        BLEQ    24$
        MOVAL   @4(R2),R2               ; R2 = ADR(AVC) FROM DESCIPTOR

24$:    MOVL    #^A/    /,(R3)+         ; FIRST CLEAR WORD
        MOVB    (R2)+,-4(R3)            ; AND PUT CHAR IN FIRST BYTE
        SOBGTR  R0,24$                  ; NEXT CHAR
99$:    RET
        .END
