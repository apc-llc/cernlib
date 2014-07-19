*
* $Id: sbyt.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: sbyt.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE SBYT  (A,X,LX,NBITS)
;#
; CERN PROGLIB# M421    SBYT            .VERSION KERNPDP  1.00  750903
;#
        ;PUTS THE FIRST 'NBITS' BITS OF A IN X AT LX
        ;LX COUNTED FROM RIGHT STARTING WITH 1
        ;ACCUMULATORS:
        MASK=1
        NBITS=2
        LX=3
        X=4
        TEMP=5
        SEARCH HELP
#include "reentr.inc"
        HELLO( SBYT)
        MOVE     NBITS,@3(16)   ;GET NBITS
        SETO     MASK,          ;PREPARE MASK
        LSH      MASK,(NBITS)
        ANDCA    MASK,@(16)    ;LOAD BITS FROM A
        MOVE     LX,@2(16)
        SETZ     TEMP,          ;PREPARE COUNTERS
        SUBI     TEMP,(NBITS)
        SUBI    TEMP,(LX)
        MOVE     X,@1(16)       ;GET A
        ROT      X,1(TEMP)
        LSH      X,(NBITS)
        MOVEM   X,SECO
        IOR     MASK,SECO
        ROT      MASK,-1(LX)    ;GET RESULT
        MOVEM    MASK,@1(16)      ; AND STORE IT AWAY
RETURN: GOODBY
#include "reent1.inc"
SECO:   0
        PRGEND
#ifdef CERNLIB_TCGEN_SBYT
#undef CERNLIB_TCGEN_SBYT
#endif
