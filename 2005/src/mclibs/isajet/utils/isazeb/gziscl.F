*
* $Id: gziscl.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gziscl.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISCL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     get pointer to first ISCL bank
C-   Returned value  :  pointer
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "izisac.inc"
#include "iziscl.inc"
      INTEGER GZISCL,GZISAC,LISAC
C----------------------------------------------------------------------
      GZISCL=0
      LISAC=GZISAC()
      IF (LISAC.NE.0  ) GZISCL=LQ(LISAC-IZISCL)
  999 RETURN
      END
