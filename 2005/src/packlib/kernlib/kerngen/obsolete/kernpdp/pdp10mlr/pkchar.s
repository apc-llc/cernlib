*
* $Id: pkchar.s,v 1.1.1.1 1996/02/15 17:53:43 mclareni Exp $
*
* $Log: pkchar.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:43  mclareni
* Kernlib
*
*
        TITLE PKCHAR (INT,CHAR,N,IPAR)
;#
; CERN PROGLIB# M427    PKCHAR          .VERSION KERNPDP  1.00  750903
;#
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
.COMMON SLATE [50]
        SEARCH HELP
#include "reentr.inc"
        HELLO( PKCHAR)
        MOVEM   P,SAV15
        MOVEM   WDCOUN,SAV17
        MOVN    N,@2(16)        ;GET N,RETURN FOR =0
        JUMPE   N,RETURN
        MOVEI   02,@3(16)       ;AC2=ADR OF IPAR
        SKIPN   03,2(02)        ;AC3 = NZONE,SKIP FOR = 0
        MOVEI   03,44           ;NZONE =36
        MOVE    NIGN,3(2)
        MOVE    NBITS,(02)      ;GET NBITS
        MOVE    06,03           ;AC6= NZONE
        SUB     06,NIGN         ;AC6=
        IDIV    06,NBITS        ;(NZONE-NIGN)/NBITS
        SKIPN   10,1(02)        ;GET NCHAR,SKIP FOR =0
        MOVE    10,06           ;NCHAR=MAXIMUM
        CAMGE   6,10            ;SET NCHAR TO THE
        MOVE    10,6            ;MAX IF NEEDED
        MOVNS   10
        MOVE    COUNT1,10
        HRLI    COUNT1,@(16)    ;COUNT1
        MOVSS   COUNT1
        MOVEI   WDCOUN,1        ;START WDCOUNT
        MOVE    NFILL,4(02)     ;GET NFILL
        HRRZI   BPT,@1(16)      ;GET ADDR OF CHAR
        MOVEM   NFILL,(BPT)     ;FILL 1ST WORD
        HRRZI   P,44            ;CALCULATE POSITION
        SUBI    P,(NIGN)        ;OF FIRST BYTE
        DPB     NBITS,[POINT  6,BPT,11]
        DPB     P    ,[POINT  6,BPT,5]
        CAIE    03,44           ;COMPARE NZONE WITH 36
        JUMPA   ,WAY2   ;THEN GO WAY2
LOOP1:  MOVE    TEMP,(COUNT1);GET INTEGER
        IDPB    TEMP,BPT        ;STORE BYTE
        AOJGE   N,FINISH        ;ASK FOR END
        AOBJN   COUNT1,LOOP1    ;ASK FOR END OF WORD
        ADDI    WDCOUN,1                ;INCREMENT WORDCOUNT
        HRL     COUNT1,NCHAR    ;RESET CHAR COUNT
        ADDI    BPT,1           ;BUMP ADDRESS
        MOVEM   NFILL,(BPT)             ;FILL NEXT WORD
        DPB     P,[POINT  6,BPT,5]
        JUMPA   LOOP1
FINISH: MOVEM   WDCOUN,SLATE
RETURN: MOVE    P,SAV15
        MOVE    WDCOUN,SAV17
        GOODBY
WAY2:   SETZ    ZERO,   ;CLEAR AC3
        SETZ    TEMP,
        SETZ    TEMP1,
        ADD     6,NCHAR ;CORRECT
        IMUL    6,NBITS ;TRAILING
        ADD     TRAIL,6 ;BITS
        ADD     TRAIL,NIGN
LOOP2:  MOVE    TEMP,(COUNT1)   ;GET INTEGER
        SUB     P,NBITS         ;PREPARE P
        JUMPL   P,WDEND         ;CHECK FOR END OF WORD
        DPB     P,[POINT  6,BPT,5]
        DPB     TEMP,BPT        ;STORE BYTE
        AOJGE   N,FINISH        ;ASK FOR END
        AOBJN   COUNT1,LOOP2    ;LOOP BACK IF NOT END OF ZONE
ZONEND: HRL     COUNT1,NCHAR    ;RESTORE COUNT
        SUB     P,TRAIL         ;JUMP OVER TRAILINGS
        JUMPGE  P,LOOP2         ;END OF WORD?
        MOVEI   P,44(P)         ;P=36-P
        ADDI    BPT,1
        ADDI    WDCOUN,1        ;BUMP WORD COUNTER
        JUMPA   ,LOOP2          ;LOOP BACK
WDEND:  LSHC    TEMP,(P)                ;SHIFT OVER END OF WORD
        ADD     P,NBITS         ;BUMP P
        DPB     P,[POINT  6,BPT,11]
        DPB     ZERO,[POINT  6,BPT,5]
        DPB     TEMP,BPT        ;DEPOSIT THE BYTE
        MOVNS   P               ;GET P= NBITS-P
        ADD     P,NBITS
        LSHC    TEMP,(P)                ;SHIFT BACK
        ADDI    BPT,1
        DPB     P,[POINT  6,BPT,11]
        SUBI    P,44                    ;CALCULATE
        MOVNS   P               ;NEW P
        DPB     P,[POINT  6,BPT,5]      ;STORE NEW POSITION
        MOVEM   NFILL,(BPT)     ;FILL NEXT WORD
        DPB     TEMP,BPT        ;STORE RIGHT HALF
        DPB     NBITS,[POINT  6,BPT,11] ;RESTORE BYTE LENGTH
        ADDI    WDCOUN,1        ;BUMP WORD COUNTER
        JUMPA   ,LOOP2+5        ;JOIN NORMAL WAY
#include "reent1.inc"
SAV15:  0
SAV17:  0
        PRGEND
#ifdef CERNLIB_TCGEN_PKCHAR
#undef CERNLIB_TCGEN_PKCHAR
#endif
