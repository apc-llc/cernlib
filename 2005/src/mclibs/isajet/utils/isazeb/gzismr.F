*
* $Id: gzismr.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzismr.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISMR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     get pointer to ISMR bank
C-   Returned value  : pointer value
C-
C-   Created  19-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISMR
#include "zebcom.inc"
#include "izismr.inc"
      INTEGER LISAC,GZISAC
C----------------------------------------------------------------------
C
      LISAC=GZISAC()
      GZISMR=0
      IF(LISAC.NE.0) GZISMR=LQ(LISAC-IZISMR)
  999 RETURN
      END
