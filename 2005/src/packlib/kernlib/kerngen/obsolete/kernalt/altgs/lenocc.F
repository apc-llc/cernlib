*
* $Id: lenocc.F,v 1.1.1.1 1996/02/15 17:50:55 mclareni Exp $
*
* $Log: lenocc.F,v $
* Revision 1.1.1.1  1996/02/15 17:50:55  mclareni
* Kernlib
*
*
#include "kernalt/pilot.h"
      FUNCTION LENOCC (CHP)
C
C CERN PROGLIB# M507    LENOCC          .VERSION KERNALT  1.11  920603
C ORIG.  02/06/92  JZ
C
C--   Find last non-blank character
C-    (With Alliant Fortran the hidden length of a string
C-     is not available to a called C routine)
 
      CHARACTER CHP*(*)
 
      LENOCC = LENOCI (CHP,LEN(CHP))
      RETURN
      END
#ifdef CERNLIB_TCGEN_LENOCC
#undef CERNLIB_TCGEN_LENOCC
#endif
