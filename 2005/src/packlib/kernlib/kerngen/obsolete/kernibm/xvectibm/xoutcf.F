*
* $Id: xoutcf.F,v 1.1.1.1 1996/02/15 17:53:09 mclareni Exp $
*
* $Log: xoutcf.F,v $
* Revision 1.1.1.1  1996/02/15 17:53:09  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE XOUTCF (LUN,BV,NB)
C
C CERN PROGLIB# Z203    XOUTCF          .VERSION KERNIBM  1.12  820329
C ORIG. 15/07/78 JZ
C
      DIMENSION    BV(NB)
C
      WRITE (LUN,8000) BV
      RETURN
C
 8000 FORMAT (200(90A4))
      END
#ifdef CERNLIB_XVECT_XOUTCF
#undef CERNLIB_XVECT_XOUTCF
#endif
