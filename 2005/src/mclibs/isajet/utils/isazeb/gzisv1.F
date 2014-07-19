*
* $Id: gzisv1.F,v 1.11 2001/10/08 14:03:16 mclareni Exp $
*
* $Log: gzisv1.F,v $
* Revision 1.11  2001/10/08 14:03:16  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      FUNCTION GZISV1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISV1
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER GZISV1
#include "zebcom.inc"
#include "izisv1.inc"
C----------------------------------------------------------------------
C
      GZISV1=0
      IF(LISAE.NE.0)  GZISV1=LQ(LISAE-IZISV1)
C
      RETURN
      END
