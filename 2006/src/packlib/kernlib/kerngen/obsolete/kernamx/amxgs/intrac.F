*
* $Id: intrac.F,v 1.1.1.1 1996/02/15 17:50:58 mclareni Exp $
*
* $Log: intrac.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:58  mclareni
* Kernlib
*
*
#include "kernamx/pilot.h"
      LOGICAL FUNCTION INTRAC()
C
C CERN PROGLIB# Z044    INTRAC          .VERSION KERNAMX  1.00  910718
C ORIG.  March 90, Roger Howard, Vancouver
C
C  Return .TRUE. if job is interactive, .FALSE. otherwise.
C  On AIX (unix) check whether standard input is a terminal.
C
      INTEGER SYSTEM
      INTRAC = SYSTEM('tty -s').EQ.0
      RETURN
      END
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
#ifdef CERNLIB_CCGEN_INTRAC
#undef CERNLIB_CCGEN_INTRAC
#endif
