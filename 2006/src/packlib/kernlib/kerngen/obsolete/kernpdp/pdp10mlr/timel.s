*
* $Id: timel.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: timel.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE TIMEL
;#
; CERN PROGLIB# Z007    TIMEL           .VERSION KERNPDP  1.02  761001
;#
;THIS ROUTINE IS ONLY USEFUL FOR JOBS WHICH RUN UNDER
;BATCH, AS THIS IS THE ONLY CASE IN WHICH TIMELIMIT IS USED.
        SEARCH HELP
#include "reentr.inc"
        T1==1
        T2==2
        T3==3
        HELLO(TIMEL)
        MOVE    T1,[XWD -1,40]
        GETTAB  T1,
        JRST    ERROR
        MOVE    T2,[POINT 12,T1,24]
        LDB     T3,T2                   ;GET TIME IN JIFFIES
        IMULI   T3,^D20                 ;MAKE MILLISECONDS
        FSC     T3,233                  ;MAKE FLOATING POINT
        FDVRI   T3,212764               ;DIVIDE BY 1000
        MOVEM   T3,@(16)
ERROR:  GOODBY
        PRGEND
#ifdef CERNLIB_TCGEN_TIMEL
#undef CERNLIB_TCGEN_TIMEL
#endif
