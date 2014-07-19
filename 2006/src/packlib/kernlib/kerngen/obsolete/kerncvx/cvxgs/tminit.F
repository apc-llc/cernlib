*
* $Id: tminit.F,v 1.1.1.1 1996/02/15 17:52:28 mclareni Exp $
*
* $Log: tminit.F,v $
* Revision 1.1.1.1  1996/02/15 17:52:28  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE TMINIT (INIT)
C
C CERN PROGLIB#         TMINIT          .VERSION KERNCVX  1.08  910806
C ORIG. 06/08/91, JZ
C Fortran interface routine to initialize TMPRO / TMREAD

C
      INIT = 7
      RETURN
      END
#ifdef CERNLIB_CCGEN_TMINIT
#undef CERNLIB_CCGEN_TMINIT
#endif
