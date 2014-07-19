*
* $Id: timex.s,v 1.1.1.1 1996/02/15 17:53:44 mclareni Exp $
*
* $Log: timex.s,v $
* Revision 1.1.1.1  1996/02/15 17:53:44  mclareni
* Kernlib
*
*
        TITLE TIMEX
;#
; CERN PROGLIB# Z007    TIMEX           .VERSION KERNPDP  1.02  761001
;#
;               FOR 50 HZ CLOCK
        SEARCH HELP
#include "reentr.inc"
        T0==1
        T1==2
        T1==2
        HELLO(TIMEX)
        PJOB    T0,                     ;GET JOBNUMBER
        JUMPE   T0,ERROR                ;NO JOB
        RUNTIM  T0,                     ;GET RUNTIME OF THE JOB
        FSC     T0,233                  ;MAKE FLOATING POINT
        FDVRI   T0,212764               ;DIVID BY 1000
        MOVEM   T0,@(16)
        GOODBY
ERROR:  OUTSTR  [ASCIZ/NO JOB
 /]
        PRGEND
#ifdef CERNLIB_TCGEN_TIMEX
#undef CERNLIB_TCGEN_TIMEX
#endif
