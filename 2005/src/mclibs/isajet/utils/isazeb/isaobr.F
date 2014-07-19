*
* $Id: isaobr.F,v 1.10 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isaobr.F,v $
* Revision 1.10  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION ISAOBR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Print out to USER.OUT begin-of-run record
C-
C-   Created   6-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      LOGICAL ISAOBR
      INTEGER USUNIT,PRUNIT
C----------------------------------------------------------------------
C
      ISAOBR=.TRUE.
      PRUNIT=USUNIT()
      CALL PRISAB(PRUNIT,0,0,0,0)
  999 RETURN
      END
