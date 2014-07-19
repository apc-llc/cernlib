*
* $Id: gzisac.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisac.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISAC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Get pointer to ISAC
C-
C-   Created   8-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "zebcom.inc"
#include "izisac.inc"
      INTEGER GZISAC
C----------------------------------------------------------------------
C
      GZISAC=0
      IF(LISAE.NE.0) GZISAC=LQ(LISAE-IZISAC)
  999 RETURN
      END
