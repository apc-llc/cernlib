*
* $Id: gzisjt.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisjt.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISJT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Find pointer to ISJT bank (jet found in toy calorimeter)
C-   Returned value  : pointer to first bank
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISJT
#include "izisjt.inc"
#include "zebcom.inc"
      INTEGER LISAC,GZISAC
C----------------------------------------------------------------------
      GZISJT=0
      LISAC=GZISAC()
      IF(LISAC.GT.0) GZISJT=LQ(LISAC-IZISJT)
  999 RETURN
      END
