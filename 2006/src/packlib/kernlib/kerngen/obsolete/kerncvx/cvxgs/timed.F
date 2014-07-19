*
* $Id: timed.F,v 1.1.1.1 1996/02/15 17:52:28 mclareni Exp $
*
* $Log: timed.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE TIMED (T)
C
C CERN PROGLIB# Z007    TIMED           .VERSION KERNCVX  1.09  910815
C ORIG. 01/12/89  KP, MM, JZ
C
C!    REAL*4      ARRAY(2),DTIME
      REAL        ARRAY(2)

      T = DTIME(ARRAY)
      RETURN
      END
#ifdef CERNLIB_TCGEN_TIMED
#undef CERNLIB_TCGEN_TIMED
#endif
#ifdef CERNLIB_CCGEN_TIMED
#undef CERNLIB_CCGEN_TIMED
#endif
