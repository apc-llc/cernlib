*
* $Id: incbyt.s,v 1.1.1.1 1996/02/15 17:53:42 mclareni Exp $
*
* $Log: incbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:42  mclareni
* Kernlib
*
*
        TITLE INCBYT (INCR,A,JTH,NBITS)
;#
; CERN PROGLIB# M423    INCBYT          .VERSION KERNPDP  1.00  750903
;#
        SEARCH HELP
#include "reentr.inc"
        HELLO( INCBYT)
        MOVEI   0,SAVAC         ;SAVE
        HRLI    0,2             ;AC-S
        BLT     0,SAVAC+13
        SETZ    0,              ;ZERO AC0
        MOVE    01,@2(16)       ;GET JTH
        MOVEI   02,@3(16)       ;GET ADDR OF NBITS
        MOVE    03,1(02)        ;AC3=NBITS(2)
        MOVEI   04,-1(01)       ;AC4=JTH-1
        IDIVI   04,(03)         ;AC4=AC4/NBITS(2)=JWORD-1
        ADDI    05,1
        MOVE    07,(02)         ;AC7=NBITS
        IMULI   05,(07)         ;AC5= (REST+1)*NBITS
        MOVEI   03,44
        SUBI    03,(05)         ;AC3=36-AC5 = LOC
        MOVEI   11,@1(16)       ;GET ADDR OF A
        ADDI    11,(04)         ;GET ADDR OF A(JWORD)
        MOVE    06,(11)         ;GET A(JWORD)
        SETZ    05,             ;ZERO AC5
        ROT     06,(03)         ;PUT THE BYTE
        LSHC    05,(07)         ;INTO AC5
        ADD     05,@(16)        ;ADD THE INCR
        MOVE    01,2(02)        ;AC1=MAX
        CAIL    01,(05)         ;COMPARE
        JRST    0,FIN           ;JUMP TO FINISH
        SUBI    05,(01)         ;AC5=SUMME -MAX
        MOVE    0,05            ;STORE OVFLOW IN AC0
        MOVE    05,01           ;STORE MAX INSTEAD OF SUMME
FIN:    MOVNS   03              ;NEGATE AC3
        MOVNS   07              ;NEGATE AC7
        LSHC    05,(07)         ;BRING BYTE
        ROT     06,(03)         ;IN RIGHT POSITION
        MOVEM   06,(11)         ;STORE RESULT
        MOVSI   1,SAVAC         ;GET BACK
        HRRI    1,2             ;AC-S
        BLT     1,15
        GOODBY
#include "reent1.inc"
SAVAC:  BLOCK 14
        PRGEND
#ifdef CERNLIB_TCGEN_INCBYT
#undef CERNLIB_TCGEN_INCBYT
#endif
