*
* $Id: whvy.F,v 1.1.1.1 1996/03/08 16:58:51 mclareni Exp $
*
* $Log: whvy.F,v $
* Revision 1.1.1.1  1996/03/08 16:58:51  mclareni
* Eurodec
*
*
#include "eurodec/pilot.h"
      FUNCTION WHVY(X)
C.----------------------------------------------------------------------
C.
C.    THE PETERSON ET AL. FUNCTION FOR HEAVY QUARKS (CHARM ETC.)
C.    LAST UPDATE: 10/04/88
C.
C.----------------------------------------------------------------------
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION X,WHVY
#endif
#include "eurodec/epsfrg.inc"
      WHVY=X*(1.0-X)**2
      WHVY=WHVY/(X*(1.0-X)-(1.0-X)-FRGEPS*X)**2
      RETURN
      END
