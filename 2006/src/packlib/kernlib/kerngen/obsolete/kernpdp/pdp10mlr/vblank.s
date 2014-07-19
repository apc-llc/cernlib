*
* $Id: vblank.s,v 1.1.1.1 1996/02/15 17:53:45 mclareni Exp $
*
* $Log: vblank.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:45  mclareni
* Kernlib
*
*
        TITLE VBLANK (VEC,N)
;#
; CERN PROGLIB# F121    VBLANK          .VERSION KERNPDP  1.00  750903
;#
        PT=1
        BLKS=2
        N=3
        SEARCH HELP
#include "reentr.inc"
        HELLO( VBLANK)
        MOVN    N,@1(16)        ;GET -N
        JUMPE   N,RETURN
        MOVE    BLKS,[ASCII /     /]    ;GET BLANKS
        MOVEI   PT,@(16)                ;GET ADDR OF VEC
        HRL     PT,N    ;LOAD LEFT HALF POINTER
LOOP:   MOVEM   BLKS,(PT)               ;STORE BLANKS
        AOBJN   PT,LOOP         ;LOOP BACK IF NOT END
RETURN: GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_VBLANK
#undef CERNLIB_TCGEN_VBLANK
#endif
