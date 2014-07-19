*
* $Id: vfill.s,v 1.1.1.1 1996/02/15 17:53:45 mclareni Exp $
*
* $Log: vfill.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:45  mclareni
* Kernlib
*
*
        TITLE VFILL (A,N,STUFF)
;#
; CERN PROGLIB# F121    VFILL           .VERSION KERNPDP  1.00  750903
;#
        PT=1
        STUFF=2
        N=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( VFILL)
        MOVN    N,@1(16)        ;GET -N
        JUMPE   N,RETURN
        MOVE    STUFF,@2(16)    ;GET STUFF
        MOVEI   PT,@(16)        ;GET ADDR OF VEC
        HRL     PT,N            ;LOAD LEFT HALF POINTER
LOOP:   MOVEM   STUFF,(PT)      ;STORE STUFF
        AOBJN   PT,LOOP         ;LOOP BACK IF NOT END
RETURN: GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_VFILL
#undef CERNLIB_TCGEN_VFILL
#endif
