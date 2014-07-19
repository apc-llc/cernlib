*
* $Id: ublow.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: ublow.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE         UBLOW  (A5,A1,N)
;#
; CERN PROGLIB# M409    UBLOW           .VERSION KERNPDP  1.02  761001
;#
        ;ACCUMULATORS:
                        BPT=0
                        COUNT=1
                        TEMP=2
                        TEMP1=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( UBLOW)
        MOVN    COUNT,@2(16)
        JUMPGE  COUNT,RETURN
        HRLZI   BPT,440700      ;INIT BYTE POINTER
        HRRI    BPT,@(16)       ;LOAD ADR. OF A5
        HRLZI   COUNT,(COUNT)   ;THE COUNTER
        HRRI    COUNT,@1(16)    ;WORD
       MOVE    TEMP,BLANK
LOOP:  ILDB    TEMP1,BPT
       DPB     TEMP1,[POINT 7,(COUNT),6]
       DPB     TEMP,[POINT 29,(COUNT),35]
        AOBJN   COUNT,LOOP      ;LOOP
RETURN: GOODBY
#include "reent1.inc"
BLANK:  ASCII/     /            ;WORD OF BLANKS
        PRGEND
#ifdef CERNLIB_TCGEN_UBLOW
#undef CERNLIB_TCGEN_UBLOW
#endif
