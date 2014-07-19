*
* $Id: upkch.s,v 1.1.1.1 1996/02/15 17:53:45 mclareni Exp $
*
* $Log: upkch.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:45  mclareni
* Kernlib
*
*
        TITLE UPKCH (CHAR,INT,N,IPAR)
;#
; CERN PROGLIB# M427    UPKCH           .VERSION KERNPDP  1.00  750903
;#
.COMMON SLATE [50]
        ;IPAR(1)= NBITS
        ;IPAR(2)= NCHAR PER ZONE
        ;IPAR(3)= NBITS PER ZONE
        ;IPAR(4)=NIGN
        ;IPAR(5)= NFILL
        ;ACCUMULATORS:
        N=1
        NFILL=2
        ZERO=3
        NIGN=4
        NBITS=5
        TRAIL=7
        NCHAR=10
        BPT=11
        COUNT1=12
        TEMP=13
        TEMP1=14
        P=15
        WDCOUN=17
        SEARCH HELP
#include "reentr.inc"
        HELLO( UPKCH)
        MOVEM   P,SAV15
        MOVEM   WDCOUN,SAV17
        MOVN    N,@2(16)        ;GET N,RETURN FOR =0
        JUMPE   N,RETURN
        MOVEI   02,@3(16)       ;AC2=ADR OF IPAR
        SKIPN   03,2(02)        ;AC3 = NZONE,SKIP FOR = 0
        MOVEI   03,44           ;NZONE =36
        MOVE    NIGN,3(02)      ;GET NIGN
        MOVE    NBITS,(02)      ;GET NBITS
        MOVE    06,03           ;AC6= NZONE
        SUB     06,NIGN         ;AC6=
        IDIV    06,NBITS        ;(NZONE-NIGN)/NBITS
        SKIPN   10,1(02)        ;GET NCHAR,SKIP FOR =0
        MOVE    10,06           ;NCHAR=MAXIMUM
        CAMGE   06,10           ;LIMIT NCHAR
        MOVE    10,06           ;TO MAX
        MOVNS   10
        MOVE    COUNT1,10
        HRLI    COUNT1,@1(16)   ;COUNT1
        MOVSS   COUNT1
        HRRZI   BPT,@(16)       ;GET ADDR OF CHAR
        HRRZI   P,44            ;CALCULATE POSITION
        SUBI    P,(NIGN)        ;OF FIRST BYTE
        DPB     NBITS,[POINT  6,BPT,11]
        DPB     P    ,[POINT  6,BPT,5]
        MOVEI   WDCOUN,1        ;START WORDCOUNT
        CAIE    03,44           ;COMPARE NZONE WITH 36
        JUMPA   ,WAY2           ;THEN GO WAY2
LOOP1:  ILDB    TEMP,BPT        ;LOAD BYTE
        MOVEM   TEMP,(COUNT1)   ;AND STORE IT
        AOJGE   N,FINISH        ;ASK FOR END
        AOBJN   COUNT1,LOOP1    ;ASK FOR END OF WORD
        ADDI    WDCOUN,1                ;INCREMENT WORDCOUNT
        HRL     COUNT1,NCHAR    ;RESET CHAR COUNT
        ADDI    BPT,1           ;BUMP ADDRESS
        DPB     P,[POINT  6,BPT,5]
        JUMPA   LOOP1
FINISH: MOVEM   WDCOUN,SLATE
RETURN: MOVE    P,SAV15
        MOVE    WDCOUN,SAV17
        GOODBY
WAY2:   SETZ    TEMP,
        SETZ    TEMP1,
        ADD     6,NCHAR         ;CORRECT
        IMUL    6,NBITS         ;TRAILING
        ADD     TRAIL,6         ;BITS
        ADD     TRAIL,NIGN
LOOP2:  SUB     P,NBITS         ;PREPARE P
        JUMPL   P,WDEND         ;CHECK FOR END OF WORD
        DPB     P,[POINT  6,BPT,5]
        LDB     TEMP,BPT        ;LOAD BYTE
        MOVEM   TEMP,(COUNT1)           ;AND STORE IT
        AOJGE   N,FINISH        ;ASK FOR END
        AOBJN   COUNT1,LOOP2    ;LOOP BACK IF NOT END OF ZONE
ZONEND: HRL     COUNT1,NCHAR    ;RESTORE COUNT
        SUB     P,TRAIL         ;JUMP OVER TRAILINGS
        JUMPGE  P,LOOP2         ;END OF WORD?
        MOVEI   P,44(P)         ;P=36-P
        ADDI    WDCOUN,1        ;BUMP WORD COUNTER
        ADDI    BPT,1
        JUMPA   ,LOOP2          ;LOOP BACK
WDEND:  SETZ    0,              ;CLEAR AC0
        SETZ    TEMP,
        DPB     0,[POINT  6,BPT,5]      ;STORE 0 FOR P
        MOVE    0,P             ;CALCULATE
        ADD     0,NBITS         ;LENGTH OF LEFT PART
        DPB     0,[POINT  6,BPT,11]
        LDB     TEMP,BPT        ;GET LEFT PART
        ADDI    BPT,1           ;BUMP ADDRESS
        MOVE    TEMP1,(BPT)     ;GET NEXT WORD
        MOVNS   P
        LSHC    TEMP,(P)        ;SHIFT RIGHT PART INTO TEMP
        DPB     NBITS,[POINT  6,BPT,11]
        MOVNS   P
        ADDI    P,44
        ADDI    WDCOUN,1        ;BUMP WORD COUNTER
        JUMPA   LOOP2+4         ;JOIN NORMAL WAY
#include "reent1.inc"
SAV15:  0
SAV17:  0
        PRGEND
#ifdef CERNLIB_TCGEN_UPKCH
#undef CERNLIB_TCGEN_UPKCH
#endif
