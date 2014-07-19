*
* $Id: oni2g.F,v 1.1.1.1 1996/03/08 16:58:52 mclareni Exp $
*
* $Log: oni2g.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:52  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION ONI2G(IP)
C.----------------------------------------------------------------------
C.
C.    MATRIX ELEMENT FOR ONIUM ---> G G DECAYS            (MATRIX = 12)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION YGLU
#endif
#include "eurodec/result.inc"
#include "eurodec/glufix.inc"
      ONI2G=1.
      IF ((PTEIL(4,NTEIL+1).LT.EGLMIN).OR.(PTEIL(4,NTEIL+2).LT.EGLMIN))
     &ONI2G=-1.
      RETURN
      END
