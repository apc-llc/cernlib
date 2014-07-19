*
* $Id: intrac.F,v 1.1.1.1 1996/02/15 17:53:49 mclareni Exp $
*
* $Log: intrac.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:49  mclareni
* Kernlib
*
*
#include "kerntmo/pilot.h"
      LOGICAL FUNCTION INTRAC(DUMMY)
C
C CERN PROGLIB# Z044    INTRAC          .VERSION KERNTMO  1.00  890208
C ORIG.  4/01/89  JZ
C
C-    The program is considered interactive if standard input
C-    is connected to the terminal
 
      LOGICAL      ISATTY
 
      INTRAC = ISATTY (5)
      RETURN
      END
#ifdef CERNLIB_TCGEN_INTRAC
#undef CERNLIB_TCGEN_INTRAC
#endif
