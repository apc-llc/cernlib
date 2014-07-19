*
* $Id: mnexin.F,v 1.1.1.1 1996/03/07 14:31:29 mclareni Exp $
*
* $Log: mnexin.F,v $
* Revision 1.1.1.1  1996/03/07 14:31:29  mclareni
* Minuit
*
*
#include "minuit/pilot.h"
      SUBROUTINE MNEXIN(PINT)
#include "minuit/d506dp.inc"
CC        Transforms the external parameter values U to internal
CC        values in the dense array PINT. Subroutine MNPINT is used.
CC
#include "minuit/d506cm.inc"
      DIMENSION PINT(*)
      LIMSET = .FALSE.
      DO 100  IINT= 1, NPAR
      IEXT = NEXOFI(IINT)
      CALL MNPINT(U(IEXT),IEXT,PINTI)
      PINT(IINT) = PINTI
  100 CONTINUE
      RETURN
      END
