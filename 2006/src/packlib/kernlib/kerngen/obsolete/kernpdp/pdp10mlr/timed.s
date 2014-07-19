*
* $Id: timed.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: timed.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE TIMED
;#
; CERN PROGLIB# Z007    TIMED           .VERSION KERNPDP  1.02  761001
;#
        SEARCH HELP
#include "reentr.inc"
        EXTERNAL TIMEX
        T1==1
        T2==2
        HELLO(TIMED)
        ENTER(TIMEX,ARG)                        ;GET RUNTIME
        MOVE    T2,TIME
        MOVE    T1,OLDT                         ;GET LAST TIME
        FSBR    T2,T1                           ;SUBTRACT
        MOVEM   T2,@(16)
        MOVE    T2,TIME                         ;GET TIME
        MOVEM   T2,OLDT                         ;STORE NEW TIME AS OLD TIME
        GOODBY
        -1
#include "reent1.inc"
ARG:    TIME
TIME:   0
OLDT:   0
        PRGEND
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
