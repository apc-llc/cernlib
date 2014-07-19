*
* $Id: vzero.s,v 1.1.1.1 1996/02/15 17:53:45 mclareni Exp $
*
* $Log: vzero.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:45  mclareni
* Kernlib
*
*
        TITLE VZERO (VEC,N)
;#
; CERN PROGLIB# F121    VZERO           .VERSION KERNPDP  1.00  750903
;#
        PT=1
        ZERO=2
        N=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( VZERO)
        MOVN    N,@1(16)        ;GET -N
        JUMPE   N,RETURN
        SETZM   0,ZERO          ;GET ZEROES
        MOVEI   PT,@(16)        ;GET ADDR OF VEC
        HRL     PT,N            ;LOAD LEFT HALF POINTER
LOOP:   MOVEM   ZERO,(PT)       ;STORE ZEROES
        AOBJN   PT,LOOP         ;LOOP BACK IF NOT END
RETURN: GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_VZERO
#undef CERNLIB_TCGEN_VZERO
#endif
