*
* $Id: tracq.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: tracq.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE TRACQ
;#
; CERN PROGLIB# N105    TRACEQ          .VERSION KERNPDP  1.00  750903
;#
#include "reentr.inc"
        ENTRY TRAC1Q,TRAC2Q
        EXTERNAL .JBREL,.JBDA
.COMMON SLATE [50]
        T1==1
        PRVLVL==2
        T2==3
        T3==4
        T4==5
        T5==6
TRAC1Q: JRST     0,.+5
        SETOM  F40
        MOVE    T1,TRAC1Q       ;F40 TAKES INITIAL ROUTINE ENTRY
                                ;FROM TRAC1Q
        MOVEM   T1,THSLVL
        JRA     16,(16)
        MOVE    PRVLVL,17               ;GET PUSHDOWN POINTER
        POP     PRVLVL,T1               ;LAST ENTRY WE DO NOT NEED
        MOVEM   PRVLVL,THSLVL   ;STORE THAT POINTER
        POPJ    17,0
TRAC2Q: JRST     0,F10
        HLRZ    T1,THSLVL       ;GET ADDRESS OF ROUTINE CALLING THIS LEVEL
        JUMPE   T1,EXIT         ;BACK AT MAIN PROGRAM IF ZERO
        CAILE   T1,.JBDA        ;CHECK THE ADDRESS IS REASONABLE
        CAMLE   T1,.JBREL
        JRST    TRCERR          ;NO GOOD
        MOVE    PRVLVL,(T1)     ;ELSE GET PREVIOUS ROUTINE ENTRY
        HRRZ    T1,THSLVL       ;GET ADDRESS TO RETURN TO 2 LEVELS BACK
CHECK:  CAILE   T1,.JBDA        ;CHECK ADRESS IS REASONABLE
        CAMLE   T1,.JBREL
        JRST    TRCERR          ;SOMETHING WRONG
        MOVEM  T1,STOCK
        MOVE   T1,F40
        CAIE   T1,0
        JRST   ROUT40
        JRST   ROUT10
RETUR:  SETZM  SLATE+2
        MOVE    T1,STOCK
        SUBI    T1,1            ;BACK OFF ONE LOCATION
        MOVEM   T1,SLATE+3      ;THAT'S THE ADDRESS
        SETZM   SLATE+4         ;AND IT IS A NORMAL RETURN
        MOVEM   PRVLVL,THSLVL   ;SO BACK ONE STEP READY FOR THE NEXT CALL
        JRST    RETURN
EXIT:   MOVEI   T1,1
        MOVEM   T1,SLATE+4
RETURN: SKIPE   F40
        JRA     16,(16)
        POPJ    17,0
TRCERR: SETOM   SLATE+4 ;TAKE ERROR RETURN
        JRST    RETURN
F10:    MOVE    PRVLVL,THSLVL   ;GET CURRENT POINTER
        POP     PRVLVL,T1               ;GET WHAT IT POINTS TO
        TLZ     T1,777777       ;CLEAR OF CALLING ADDRESS
        HLRZ    T2,PRVLVL               ;TEST FOR LAST OF STACK
        CAIN    T2,-100
        JRST    EXIT
        MOVEM   PRVLVL,THSLVL
        JRST    CHECK
ROUT40: SETZM  SLATE
        SETZM  SLATE+1
        JRST   RETUR
ROUT10: MOVE   T1,THSLVL
        MOVEM  T1,COUNT
        HRRZ   T2,COUNT
        MOVE   T1,(T2)
        MOVEM  T1,COUNT
        HRRZ   T3,COUNT
        SOJ    T3,
        MOVE   T2,(T3)
        MOVEM  T2,COUNT
        HRRZ   T1,COUNT
        SOJ    T1,
        SETZM  COUNT
        MOVE   T2,(T1)
        MOVE   T4,[POINT 7,SLATE]
        MOVE   T3,[POINT 6,T2]
NEXTCH: ILDB   T1,T3
        ADDI   T1,40
        IDPB   T1,T4
        AOS    T5,COUNT
        CAIE   T5,5
        JRST   NEXTCH
        MOVE   T4,[POINT 7,SLATE+1]
        ILDB   T1,T3
        ADDI   T1,40
        IDPB   T1,T4
        JRST   RETUR
#include "reent1.inc"
STOCK:  0
COUNT:  0
F40:    0
THSLVL: 0
       PRGEND
