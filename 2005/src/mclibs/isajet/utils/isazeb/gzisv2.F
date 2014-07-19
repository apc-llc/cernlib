*
* $Id: gzisv2.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisv2.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISV2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISV2
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISV2
#include "zebcom.inc"
#include "izisv2.inc"
C----------------------------------------------------------------------
C
      GZISV2=0
      IF(LISAE.NE.0)  GZISV2=LQ(LISAE-IZISV2)
C
      RETURN
      END
