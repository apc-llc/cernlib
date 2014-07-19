*
* $Id: jbyt.s,v 1.1.1.1 1996/02/15 17:53:43 mclareni Exp $
*
* $Log: jbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:43  mclareni
* Kernlib
*
*
        TITLE JBYT  (A,LA,NBITS)
;#
; CERN PROGLIB# M421    JBYT            .VERSION KERNPDP  1.00  750903
;#
                ;FUNCTION
                ;TAKES 'NBITS' BITS FROM A AT POSITION LA
                ;RETURNS THEM IN FIRST POSITION
                ;REST OF FUNCTION VALUE IS ZERO
        ;ACCUMULATORS:
        LA=1
        NBITS=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( JBYT)
        MOVEM     3,SAVAC       ;TO SAVE AC
        MOVE     NBITS,@2(16)   ;LOAD NBITS
        MOVE     0,@(16)        ;PREPARE COUNTERS
        MOVN     LA,@1(16)
        SUBI      LA,(NBITS)
        LSH      0,45(LA)       ;SHIFT OUT LEFTMOST BITS
        LSH      0,-44(NBITS)   ;SHIFT OUT RIGHTMOST BITS
        MOVE     3,SAVAC        ;STORE BACK AC
        GOODBY
#include "reent1.inc"
  SAVAC: 0
        PRGEND
#ifdef CERNLIB_TCGEN_JBYT
#undef CERNLIB_TCGEN_JBYT
#endif
